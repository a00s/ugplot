# ugplot

ugPlot is an R library to be used as pipeline for testing caret models in the context of Epitage.

## Instalação com 1 comando (Podman + r-universe)

A instalação do `ugplot` depende de pacotes do CRAN **e** do Bioconductor (ex.: `ConsensusClusterPlus`).
Em container limpo, também são necessários headers de sistema para compilar dependências como `httr`, `xml2`, `sass` e `curl`.

```bash
podman run --rm -it \
  -p 127.0.0.1:8787:8787 \
  -e DISABLE_AUTH=true \
  docker.io/rocker/rstudio:latest \
  bash -lc 'apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    libharfbuzz-dev libfribidi-dev && \
    R -q -e "install.packages(\"BiocManager\", repos=\"https://cloud.r-project.org\"); \
    options(repos=c(a00s=\"https://a00s.r-universe.dev\", BiocManager::repositories())); \
    install.packages(\"ugplot\")"'
```

Depois de instalar, no R:

```r
library(ugplot)
ugPlot()
```
