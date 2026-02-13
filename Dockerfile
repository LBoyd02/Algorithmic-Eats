FROM rocker/shiny-verse:latest


RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    build-essential \
    pkg-config \
    cmake \
    libudunits2-dev \
    libgdal-dev gdal-bin \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
  && rm -rf /var/lib/apt/lists/*

RUN git clone --depth 1 https://github.com/LBoyd02/Algorithmic-Eats.git \
    /srv/shiny-server/Algorithmic-Eats

RUN Rscript /srv/shiny-server/Algorithmic-Eats/requirements.R

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/Algorithmic-Eats/app', host='0.0.0.0', port=3838)"]
