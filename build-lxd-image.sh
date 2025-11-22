#!/bin/bash
# Build LXD/Incus image with pconch pre-installed

set -e

IMAGE_NAME="pconch"
IMAGE_ALIAS="pconch/trixie"

echo "=== Building .deb package ==="
./build-deb.sh

echo ""
echo "=== Creating LXD container from base Debian Trixie ==="
incus launch images:debian/trixie/amd64 pconch-builder

echo ""
echo "=== Waiting for container to be ready ==="
sleep 5

echo ""
echo "=== Copying .deb package to container ==="
DEB_FILE=$(ls deb-output/*.deb | head -1)
incus file push "$DEB_FILE" pconch-builder/tmp/

echo ""
echo "=== Installing pconch package ==="
incus exec pconch-builder -- apt-get update
incus exec pconch-builder -- apt-get install -y "/tmp/$(basename $DEB_FILE)"

echo ""
echo "=== Cleaning up ==="
incus exec pconch-builder -- apt-get clean
incus exec pconch-builder -- rm -rf /var/lib/apt/lists/* /tmp/*.deb

echo ""
echo "=== Stopping container ==="
incus stop pconch-builder

echo ""
echo "=== Publishing as image ==="
incus publish pconch-builder --alias "$IMAGE_ALIAS" description="Debian Trixie with pconch $(date +%Y-%m-%d)"

echo ""
echo "=== Cleaning up builder container ==="
incus delete pconch-builder

echo ""
echo "=== Image ready! ==="
echo "Launch with: incus launch $IMAGE_ALIAS myapp"
