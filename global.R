library(shiny)
library(bslib)
library(bsicons)
library(shinylogs)
library(tidyverse)
library(lubridate)
library(magrittr)
library(waiter)
library(sever)
library(shinyWidgets)
library(leaflet)
library(leaflet.minicharts)
library(highcharter)
library(gtsummary)
library(shinyscreenshot)
library(timevis)
source(here::here("R", "utils_data.R"))

app_name <- "gtfcc_ocv"
app_title <- "GTFCC OCV"
app_font <- "Alegreya Sans"
p_font <- "Merriweather"

SUCCESS <- "#19bdb4"
WARNING <- "#d39234"

# local disk cache
shiny::shinyOptions(cache = cachem::cache_disk(here::here(".cache")))
 
# week starts monday
options("lubridate.week.start" = 1)

date_updated <- readr::read_rds(here::here("data", "date_updated.rds")) %>% format("%d %B %Y")
sf_world <- readr::read_rds(here::here("data", "sf_world.rds"))
app_data <- readr::read_rds(here::here("data", "app_data.rds"))
df_request <- app_data$request
df_round <- app_data$campaign_and_round
df_shipment <- app_data$shipment
df_timevis <- app_data$df_timevis

q_range <- get_q_range(c(
  df_request$r_date_receipt,
  df_shipment$s_date_delivery,
  df_round$cr_date_round_start
))

 disconnected <- sever::sever_default(
  title = "Disconnected",
  subtitle = "Sorry your session timed-out or something went wrong",
  button = "Reconnect",
  button_class = "info"
)
