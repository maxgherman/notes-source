#!/bin/sh
set -eu

host="${1:-server}"
output_directory="${2:-certs}"

mkdir -p "$output_directory"
for path in ca.key ca.crt server.key server.csr server.crt; do
    if [ -e "$output_directory/$path" ]; then
        echo "$output_directory/$path already exists; use a new directory or remove the old test certificates" >&2
        exit 1
    fi
done

case "$host" in
    *[!0-9.]* | *.*.*.*.*) subject_alt_name="DNS:$host" ;;
    *) subject_alt_name="IP:$host" ;;
esac

umask 077
openssl req -x509 -newkey rsa:2048 -nodes -days 7 \
    -subj "/CN=Million RPS Benchmark Test CA" \
    -keyout "$output_directory/ca.key" \
    -out "$output_directory/ca.crt"

openssl req -newkey rsa:2048 -nodes \
    -subj "/CN=$host" \
    -addext "subjectAltName=$subject_alt_name" \
    -keyout "$output_directory/server.key" \
    -out "$output_directory/server.csr"

openssl x509 -req -days 7 \
    -in "$output_directory/server.csr" \
    -CA "$output_directory/ca.crt" \
    -CAkey "$output_directory/ca.key" \
    -CAcreateserial \
    -copy_extensions copy \
    -out "$output_directory/server.crt"

echo "created a seven-day test CA and server certificate for $host in $output_directory"
