use std::{
    fs::File,
    io::BufReader,
    net::SocketAddr,
    path::Path,
    pin::Pin,
    sync::{
        Arc,
        atomic::{AtomicU64, Ordering},
    },
    task::{Context, Poll},
};

use rustls::{ClientConfig, RootCertStore, pki_types::ServerName};
use thiserror::Error;
use tokio::{
    io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, ReadBuf},
    net::{TcpStream, lookup_host},
};
use tokio_rustls::TlsConnector;
use url::Url;

const MAX_HEADER_BYTES: usize = 64 * 1024;
const MAX_BODY_BYTES: usize = 2 * 1024 * 1024;

#[derive(Debug, Error)]
pub enum HttpError {
    #[error("I/O failed: {0}")]
    Io(#[from] std::io::Error),
    #[error("TLS configuration failed: {0}")]
    TlsConfig(String),
    #[error("TLS handshake failed: {0}")]
    Tls(#[from] rustls::Error),
    #[error("invalid HTTP response: {0}")]
    Protocol(String),
}

pub struct Target {
    address: SocketAddr,
    host: String,
    host_header: String,
    tls: Option<Arc<ClientConfig>>,
    connection_attempts: AtomicU64,
    connections_opened: AtomicU64,
}

pub struct HttpConnection {
    stream: Stream,
    host_header: String,
    buffer: Vec<u8>,
}

pub struct HttpResponse {
    pub status: u16,
    pub body: Vec<u8>,
    pub reusable: bool,
}

impl Target {
    pub async fn new(url: &Url, ca_cert: Option<&Path>) -> Result<Self, HttpError> {
        let host = url
            .host_str()
            .ok_or_else(|| HttpError::Protocol("target has no host".into()))?
            .to_owned();
        let port = url
            .port_or_known_default()
            .ok_or_else(|| HttpError::Protocol("target has no known port".into()))?;
        // Resolve once so a reconnect storm cannot turn into a Route 53
        // link-local traffic storm. Keep `host` separately for HTTP Host and
        // TLS SNI/certificate validation.
        let address = lookup_host((host.as_str(), port))
            .await?
            .next()
            .ok_or_else(|| {
                HttpError::Io(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("target host {host} resolved to no addresses"),
                ))
            })?;
        let host_header = match url.port() {
            Some(port) => format!("{host}:{port}"),
            None => host.clone(),
        };
        let tls = if url.scheme() == "https" {
            let ca_cert = ca_cert.ok_or_else(|| {
                HttpError::TlsConfig("a CA certificate is required for HTTPS".into())
            })?;
            Some(load_client_config(ca_cert)?)
        } else {
            None
        };
        Ok(Self {
            address,
            host,
            host_header,
            tls,
            connection_attempts: AtomicU64::new(0),
            connections_opened: AtomicU64::new(0),
        })
    }

    pub async fn connect(&self) -> Result<HttpConnection, HttpError> {
        self.connection_attempts.fetch_add(1, Ordering::Relaxed);
        let tcp = TcpStream::connect(self.address).await?;
        tcp.set_nodelay(true)?;
        let stream = if let Some(config) = &self.tls {
            let server_name = ServerName::try_from(self.host.clone())
                .map_err(|error| HttpError::TlsConfig(error.to_string()))?;
            let tls = TlsConnector::from(config.clone())
                .connect(server_name, tcp)
                .await?;
            Stream::Tls(Box::new(tls))
        } else {
            Stream::Plain(tcp)
        };
        self.connections_opened.fetch_add(1, Ordering::Relaxed);
        Ok(HttpConnection {
            stream,
            host_header: self.host_header.clone(),
            buffer: Vec::with_capacity(16 * 1024),
        })
    }

    pub fn connection_counts(&self) -> (u64, u64) {
        (
            self.connection_attempts.load(Ordering::Relaxed),
            self.connections_opened.load(Ordering::Relaxed),
        )
    }
}

impl HttpConnection {
    pub async fn get(&mut self, path: &str) -> Result<HttpResponse, HttpError> {
        let request = format!(
            "GET {path} HTTP/1.1\r\nHost: {}\r\nAccept: application/json\r\nConnection: keep-alive\r\n\r\n",
            self.host_header
        );
        self.stream.write_all(request.as_bytes()).await?;
        self.stream.flush().await?;
        self.read_response().await
    }

    async fn read_response(&mut self) -> Result<HttpResponse, HttpError> {
        loop {
            if let Some(header_end) = find_header_end(&self.buffer) {
                let (status, content_length, reusable) = parse_headers(&self.buffer[..header_end])?;
                let message_end = header_end
                    .checked_add(content_length)
                    .ok_or_else(|| HttpError::Protocol("response length overflow".into()))?;
                if content_length > MAX_BODY_BYTES {
                    return Err(HttpError::Protocol(format!(
                        "response body exceeds {MAX_BODY_BYTES} bytes"
                    )));
                }
                if self.buffer.len() >= message_end {
                    let body = self.buffer[header_end..message_end].to_vec();
                    self.buffer.drain(..message_end);
                    return Ok(HttpResponse {
                        status,
                        body,
                        reusable,
                    });
                }
            } else if self.buffer.len() > MAX_HEADER_BYTES {
                return Err(HttpError::Protocol(format!(
                    "response headers exceed {MAX_HEADER_BYTES} bytes"
                )));
            }

            let read = self.stream.read_buf(&mut self.buffer).await?;
            if read == 0 {
                return Err(HttpError::Protocol(
                    "connection closed before the complete response arrived".into(),
                ));
            }
        }
    }
}

fn load_client_config(path: &Path) -> Result<Arc<ClientConfig>, HttpError> {
    let mut reader = BufReader::new(File::open(path)?);
    let certs = rustls_pemfile::certs(&mut reader).collect::<Result<Vec<_>, _>>()?;
    if certs.is_empty() {
        return Err(HttpError::TlsConfig(format!(
            "no certificates found in {}",
            path.display()
        )));
    }
    let mut roots = RootCertStore::empty();
    for cert in certs {
        roots
            .add(cert)
            .map_err(|error| HttpError::TlsConfig(error.to_string()))?;
    }
    let provider = rustls::crypto::ring::default_provider();
    let config = ClientConfig::builder_with_provider(Arc::new(provider))
        .with_protocol_versions(&[&rustls::version::TLS13])
        .map_err(|error| HttpError::TlsConfig(error.to_string()))?
        .with_root_certificates(roots)
        .with_no_client_auth();
    Ok(Arc::new(config))
}

fn find_header_end(buffer: &[u8]) -> Option<usize> {
    buffer
        .windows(4)
        .position(|window| window == b"\r\n\r\n")
        .map(|position| position + 4)
}

fn parse_headers(headers: &[u8]) -> Result<(u16, usize, bool), HttpError> {
    let mut parsed_headers = [httparse::EMPTY_HEADER; 32];
    let mut response = httparse::Response::new(&mut parsed_headers);
    match response
        .parse(headers)
        .map_err(|error| HttpError::Protocol(error.to_string()))?
    {
        httparse::Status::Complete(_) => {}
        httparse::Status::Partial => {
            return Err(HttpError::Protocol("partial response headers".into()));
        }
    }
    let status = response
        .code
        .ok_or_else(|| HttpError::Protocol("response has no status".into()))?;
    let mut content_length = None;
    let mut reusable = true;
    for header in response.headers.iter() {
        if header.name.eq_ignore_ascii_case("content-length") {
            let value = std::str::from_utf8(header.value)
                .map_err(|error| HttpError::Protocol(error.to_string()))?;
            content_length = Some(
                value
                    .trim()
                    .parse::<usize>()
                    .map_err(|error| HttpError::Protocol(error.to_string()))?,
            );
        }
        if header.name.eq_ignore_ascii_case("transfer-encoding") {
            return Err(HttpError::Protocol(
                "chunked responses are not supported by the benchmark contract".into(),
            ));
        }
        if header.name.eq_ignore_ascii_case("connection")
            && header.value.eq_ignore_ascii_case(b"close")
        {
            reusable = false;
        }
    }
    let content_length = content_length
        .ok_or_else(|| HttpError::Protocol("response has no Content-Length".into()))?;
    Ok((status, content_length, reusable))
}

enum Stream {
    Plain(TcpStream),
    Tls(Box<tokio_rustls::client::TlsStream<TcpStream>>),
}

impl AsyncRead for Stream {
    fn poll_read(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
        buffer: &mut ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        match self.get_mut() {
            Self::Plain(stream) => Pin::new(stream).poll_read(context, buffer),
            Self::Tls(stream) => Pin::new(stream.as_mut()).poll_read(context, buffer),
        }
    }
}

impl AsyncWrite for Stream {
    fn poll_write(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
        buffer: &[u8],
    ) -> Poll<Result<usize, std::io::Error>> {
        match self.get_mut() {
            Self::Plain(stream) => Pin::new(stream).poll_write(context, buffer),
            Self::Tls(stream) => Pin::new(stream.as_mut()).poll_write(context, buffer),
        }
    }

    fn poll_flush(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        match self.get_mut() {
            Self::Plain(stream) => Pin::new(stream).poll_flush(context),
            Self::Tls(stream) => Pin::new(stream.as_mut()).poll_flush(context),
        }
    }

    fn poll_shutdown(
        self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        match self.get_mut() {
            Self::Plain(stream) => Pin::new(stream).poll_shutdown(context),
            Self::Tls(stream) => Pin::new(stream.as_mut()).poll_shutdown(context),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_content_length_and_connection_reuse() {
        let headers = b"HTTP/1.1 200 OK\r\nContent-Length: 12\r\nConnection: close\r\n\r\n";
        assert_eq!(parse_headers(headers).unwrap(), (200, 12, false));
    }

    #[test]
    fn rejects_chunked_responses() {
        let headers = b"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n";
        assert!(parse_headers(headers).is_err());
    }
}
