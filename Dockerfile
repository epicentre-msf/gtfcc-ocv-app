FROM epicentremsf/shinytvgeo:latest

LABEL maintainer="Paul Campbell paul.campbell@epicentre.msf.org"

# install packages required for shiny apps
RUN R -e 'remotes::install_cran("timevis")'
RUN R -e 'remotes::install_github("rstudio/bslib")'
RUN R -e 'remotes::install_github("rstudio/bsicons")'
RUN R -e 'remotes::install_cran("htmlwidgets")'
RUN R -e 'remotes::install_cran("shinyscreenshot")'
