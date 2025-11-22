#!/bin/bash
# Build LXD image using distrobuilder

set -e

echo "=== Building .deb package ==="
./build-deb.sh

echo ""
echo "=== Preparing for distrobuilder ==="
DEB_FILE=$(ls deb-output/*.deb | head -1)
mkdir -p /tmp/pconch-build
cp "$DEB_FILE" /tmp/pconch-build/pconch.deb

echo ""
echo "=== Building image with distrobuilder ==="
cd /tmp/pconch-build
sudo distrobuilder build-incus distrobuilder.yaml

echo ""
echo "=== Importing image to Incus ==="
incus image import incus.tar.xz rootfs.squashfs --alias pconch/trixie

echo ""
echo "=== Cleaning up ==="
cd -
sudo rm -rf /tmp/pconch-build

echo ""
echo "=== Image ready! ==="
echo "Launch with: incus launch pconch/trixie myapp"
