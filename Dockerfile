FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y git

RUN git clone https://github.com/LBoyd02/Algorithmic-Eats.git /srv/shiny-server/Algorithmic-Eats
RUN Rscript /srv/shiny-server/Algorithmic-Eats/requirements.R

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/Algorithmic-Eats/app', host = '0.0.0.0', port = 3838)"]