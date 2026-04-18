# ugplot Installation Guide (English)

This guide provides a reproducible installation flow for `ugplot`, including OS-level libraries and R dependencies that commonly fail in clean environments.

## 1) System dependencies (Linux)

`ugplot` depends on packages that compile native code (for example `fs`, `curl`, `sass`, `xml2`, graphics stack packages). Install build tools and development headers first.

### Debian / Ubuntu

```bash
sudo apt-get update
sudo apt-get install -y --no-install-recommends \
  build-essential gfortran pkg-config \
  libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev \
  libuv1-dev \
  libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
  libcairo2-dev libharfbuzz-dev libfribidi-dev
```

### Fedora / RHEL / CentOS (dnf)

```bash
sudo dnf install -y \
  gcc gcc-c++ gcc-gfortran make pkgconf-pkg-config \
  libcurl-devel openssl-devel libxml2-devel libgit2-devel \
  libuv-devel \
  fontconfig-devel freetype-devel libpng-devel libtiff-devel libjpeg-turbo-devel \
  cairo-devel harfbuzz-devel fribidi-devel
```

## 2) Install R repositories and package dependencies

`ugplot` uses CRAN + Bioconductor dependencies (notably `ConsensusClusterPlus`) and is published on r-universe.

Run the following in R:

```r
# CRAN first for installer tooling
install.packages("BiocManager", repos = "https://cloud.r-project.org")

# Configure repositories: ugplot r-universe + CRAN + Bioconductor
options(repos = c(
  a00s = "https://a00s.r-universe.dev",
  CRAN = "https://cloud.r-project.org",
  BiocManager::repositories()
))

# Pre-install common transitive dependencies that often fail in minimal images
install.packages(c(
  "fs", "curl", "sass", "bslib", "rmarkdown", "httpuv", "httr",
  "htmlwidgets", "miniUI", "colourpicker"
))

# Bioconductor dependency used by ugplot
BiocManager::install("ConsensusClusterPlus", ask = FALSE, update = FALSE)

# Install ugplot
install.packages("ugplot")
```

## 3) Validate installation

```r
library(ugplot)
ugPlot()
```

## 4) One-command install in a clean container (Podman)

```bash
podman run --rm -it \
  -p 127.0.0.1:8787:8787 \
  -e DISABLE_AUTH=true \
  docker.io/rocker/rstudio:latest \
  bash -lc 'apt-get update && apt-get install -y --no-install-recommends \
    build-essential gfortran pkg-config \
    libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev libuv1-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    libcairo2-dev libharfbuzz-dev libfribidi-dev && \
    R -q -e "install.packages(\"BiocManager\", repos=\"https://cloud.r-project.org\"); \
    options(repos=c(a00s=\"https://a00s.r-universe.dev\", CRAN=\"https://cloud.r-project.org\", BiocManager::repositories())); \
    install.packages(c(\"fs\",\"curl\",\"sass\",\"bslib\",\"rmarkdown\",\"httpuv\",\"httr\",\"htmlwidgets\",\"miniUI\",\"colourpicker\")); \
    BiocManager::install(\"ConsensusClusterPlus\", ask=FALSE, update=FALSE); \
    install.packages(\"ugplot\")"'
```

## Notes

- If installation still fails, run `sessionInfo()` and keep the first package that fails compilation; usually this points to a missing OS-level `-dev` package.
- In CI, always install system dependencies before running `install.packages("ugplot")`.
