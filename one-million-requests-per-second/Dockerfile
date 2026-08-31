FROM rust:1.94-bookworm AS builder
WORKDIR /build

COPY Cargo.toml Cargo.lock ./
COPY crates ./crates
RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/build/target \
    cargo test --workspace --locked \
    && cargo build --workspace --release --locked \
    && cp /build/target/release/million-rps-server /tmp/million-rps-server \
    && cp /build/target/release/million-rps-loadgen /tmp/million-rps-loadgen \
    && cp /build/target/release/million-rps-dbtool /tmp/million-rps-dbtool

FROM debian:bookworm-slim
COPY --from=builder /tmp/million-rps-server /usr/local/bin/million-rps-server
COPY --from=builder /tmp/million-rps-loadgen /usr/local/bin/million-rps-loadgen
COPY --from=builder /tmp/million-rps-dbtool /usr/local/bin/million-rps-dbtool

USER 65532:65532
EXPOSE 8080
ENTRYPOINT ["million-rps-server"]
