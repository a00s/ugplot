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

# Instala remotes e dependências do DESCRIPTION
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org')" && \
    R -e "remotes::install_deps(dependencies = TRUE, repos = 'https://cloud.r-project.org')"

# Porta padrão do Shiny
EXPOSE 3838

# Sobe o app (ajuste se seu entrypoint for outro)
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/ugplot/R/app.R', host='0.0.0.0', port=3838)"]
