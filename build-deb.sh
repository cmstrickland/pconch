#!/bin/bash
# Build .deb package using Docker

set -e

echo "Building pconch .deb package for Debian Trixie..."
echo

# Create output directory for .deb files
mkdir -p deb-output

# Build the Docker image
docker build -f Dockerfile.deb-builder -t pconch-deb-builder .

# Run the container and extract the .deb files
# The .deb files are created in the parent directory of /build
docker run --rm -v "$(pwd)/deb-output:/output" pconch-deb-builder bash -c '
    echo "Copying .deb files to output directory..."
    cp ../*.deb /output/ 2>/dev/null || true
    cp ../*.changes /output/ 2>/dev/null || true
    cp ../*.buildinfo /output/ 2>/dev/null || true
    ls -lh /output/
'

echo
echo "Build complete! .deb files are in ./deb-output/"
ls -lh deb-output/
