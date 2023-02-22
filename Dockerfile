FROM epicentremsf/shinytvgeo:latest

LABEL maintainer="Paul Campbell paul.campbell@epicentre.msf.org"

# install packages required for shiny apps
RUN R -e 'remotes::install_cran("timevis")'