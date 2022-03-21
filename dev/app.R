library(shiny)
library(shinylogs)
library(tidyverse)
library(lubridate)
library(magrittr)
library(waiter)
library(sever)
library(cicerone)
library(shinyWidgets)
library(shinydashboard)

app_name <- "gtfcc_ocv"
app_title <- "GTFCC OCV"
app_font <- "Alegreya Sans"

# local disk cache
# shiny::shinyOptions(cache = cachem::cache_disk(here::here(".cache")))

# Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
# options("lubridate.week.start" = 1)
# aweek::set_week_start("Monday")

# sf_adm2 <- read_rds(here::here("data", "sf_adm2.rds"))


# ==================================================
# USER INTERFACE

ui <- app_ui()

# ==================================================
# APP SERVER

server <- app_server()

# ==================================================
# LAUNCH APPLICATION 
shinyApp(ui, server)

