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

# Instala dependências e tenta instalar o pacote local.
# Se a instalação do pacote falhar, o container ainda sobe via source() (fallback no CMD).
RUN Rscript -e "install.packages(c('remotes','BiocManager'), repos='https://cloud.r-project.org')" && \
    Rscript -e "BiocManager::install(c('ConsensusClusterPlus','methyLImp2'), ask = FALSE, update = FALSE)" && \
    Rscript -e "remotes::install_deps('/srv/shiny-server/ugplot', dependencies = TRUE, repos = BiocManager::repositories())" && \
    Rscript -e "try(remotes::install_local('/srv/shiny-server/ugplot', dependencies = FALSE, upgrade = 'never', repos = BiocManager::repositories()), silent = TRUE)"

# Porta padrão do Shiny
EXPOSE 3838

# Sobe o app priorizando o pacote instalado; se não existir, usa o código-fonte local.
CMD ["Rscript", "-e", "if (requireNamespace('ugplot', quietly = TRUE)) { app <- ugplot::ugPlot() } else { source('/srv/shiny-server/ugplot/R/app.R'); app <- ugPlot() }; shiny::runApp(app, host='0.0.0.0', port=3838)"]
