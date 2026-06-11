FROM rocker/shiny:4.4.0

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('bslib', 'dplyr','tidyr','lubridate', 'readr', 'DT', 'plotly', 'shiny.i18n', 'DBI', 'RSQLite', 'bsicons'), repos='https://cloud.r-project.org/')"

# Copy app files
RUN rm -rf /srv/shiny-server/*
COPY . /srv/shiny-server/

# Expose and run on Railway's dynamic PORT
EXPOSE ${PORT:-3838}

CMD R -e "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '3838')))"
