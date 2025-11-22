# Building LXD/Incus Images for pconch

This document describes how to build LXD/Incus container images with pconch pre-installed.

## Prerequisites

- Incus/LXD installed and configured
- Podman for building the .deb package
- (For distrobuilder approach) distrobuilder installed

## Approach 1: Simple Script (Recommended for quick start)

Uses a running container to install the .deb and publish as an image.

```bash
./build-lxd-image.sh
```

This will:
1. Build the .deb package using Podman
2. Launch a Debian Trixie container
3. Install the .deb package
4. Publish as an image with alias `pconch/trixie`

**Usage:**
```bash
# Launch a new container with pconch
incus launch pconch/trixie my-pconch-app

# Start pconch service
incus exec my-pconch-app -- systemctl start pconch
```

## Approach 2: Distrobuilder (Recommended for production)

Uses distrobuilder to create a properly structured LXD image from scratch.

```bash
./build-with-distrobuilder.sh
```

**Advantages:**
- Cleaner, reproducible builds
- Better metadata
- More control over image creation
- Follows LXD best practices

## Updating the Image

To update the image with a new version of pconch:

```bash
# Rebuild and republish
./build-lxd-image.sh

# Existing containers won't be affected
# Launch new containers to use the updated image
incus launch pconch/trixie my-new-app
```

## Configuration

Edit the scripts to customize:
- Image name/alias
- Additional packages to install
- Post-install configuration
- Service enablement

## CI/CD Integration

These scripts can be integrated into CI/CD pipelines:

```bash
# In your CI pipeline:
./build-deb.sh           # Build .deb
./build-lxd-image.sh     # Create LXD image
# Push image to registry or use directly
```
