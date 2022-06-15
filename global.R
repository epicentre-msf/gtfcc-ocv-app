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
library(shinytreeview)
library(leaflet)
library(leaflet.minicharts)
library(highcharter)
library(capture)
library(timevis)
source(here::here("R", "utils_data.R"))

app_name <- "gtfcc_ocv"
app_title <- "GTFCC OCV"
app_font <- "Alegreya Sans"

# local disk cache
shiny::shinyOptions(cache = cachem::cache_disk(here::here(".cache")))

# Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
options("lubridate.week.start" = 1)
# aweek::set_week_start("Monday")

sf_world <- readr::read_rds(here::here("data", "sf_world.rds"))
app_data <- readr::read_rds(here::here("data", "app_data.rds"))
df_request <- app_data$request
df_round <- app_data$round
df_shipment <- app_data$shipment
df_timevis <- app_data$df_timevis

q_range <- range(df_request$date_receipt, na.rm = TRUE) %>% 
  floor_date("quarter") %>% 
  as_date()
q_range <- seq.Date(q_range[1], q_range[2], by = "3 months") %>% 
  quarter(with_year = TRUE) %>% 
  str_replace("\\.", "-Q")

disconnected <- sever::sever_default(
  title = "Oops!",
  subtitle = "Sorry something went wrong or your session timed-out",
  button = "Reconnect",
  button_class = "info"
)
