# ugplot

ugPlot is an R library to test caret-based models in Epitage pipelines.

## Documentation and installation

For full installation details (system libraries, R dependencies, and troubleshooting), see [`INSTALLATION.md`](INSTALLATION.md).

Quick install in a clean Podman container:

```bash
podman run -it -p 127.0.0.1:8787:8787 -e DISABLE_AUTH=true docker.io/rocker/rstudio:latest bash -lc 'apt-get update && apt-get install -y --no-install-recommends build-essential gfortran pkg-config libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev libuv1-dev libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libcairo2-dev libharfbuzz-dev libfribidi-dev && R -q -e "install.packages(\"BiocManager\", repos=\"https://cloud.r-project.org\"); options(repos=c(a00s=\"https://a00s.r-universe.dev\", CRAN=\"https://cloud.r-project.org\", BiocManager::repositories())); install.packages(c(\"fs\",\"curl\",\"sass\",\"bslib\",\"rmarkdown\",\"httpuv\",\"httr\",\"htmlwidgets\",\"miniUI\",\"colourpicker\")); BiocManager::install(\"ConsensusClusterPlus\", ask=FALSE, update=FALSE); install.packages(\"ugplot\")"'
```

After installation, run in R:

```r
library(ugplot)
ugPlot()
```
