# Base com R + Shiny já pronto
FROM rocker/shiny:4.3.3

# Dependências de sistema (ajuste se seu pacote precisar de mais libs)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Copia o projeto
WORKDIR /srv/shiny-server/ugplot
COPY . .

# Instala remotes e o pacote local (incluindo inst/extdata)
RUN R -e "install.packages(c('remotes','BiocManager'), repos='https://cloud.r-project.org')" && \
    R -e "BiocManager::install('ConsensusClusterPlus', ask = FALSE, update = FALSE)" && \
    R -e "remotes::install_local('/srv/shiny-server/ugplot', dependencies = TRUE, upgrade = 'never', repos = 'https://cloud.r-project.org')"

# Porta padrão do Shiny
EXPOSE 3838

# Sobe o app a partir do pacote instalado
CMD ["R", "-e", "app <- ugplot::ugPlot(); shiny::runApp(app, host='0.0.0.0', port=3838)"]
