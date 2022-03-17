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

app_name <- "test_app"
app_title <- "Test"

# local disk cache
shiny::shinyOptions(cache = cachem::cache_disk(here::here(".cache")))

# Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
# options("lubridate.week.start" = 1)
# aweek::set_week_start("Monday")

# sf_adm2 <- read_rds(here::here("data", "sf_adm2.rds"))

# connect to local DB 
con <- RSQLite::dbConnect(RSQLite::SQLite(), here::here("data", "local.db"))

shiny::onStop(function() {
  DBI::dbDisconnect(con)
})

disconnected <- sever_default(
  title = "Oops!",
  subtitle = "Sorry something went wrong or your session timed-out",
  button = "Reconnect",
  button_class = "info"
)

# ==================================================
# USER INTERFACE

ui <- tagList(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans+Condensed:wght@300;400;700&display=swap", rel = "stylesheet"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "addNavLink.js"),
    shinyjs::useShinyjs(),
    shinyWidgets::useShinydashboard(),
    waiter::use_waiter(),
    sever::use_sever(),
    cicerone::use_cicerone()
  ),
  
  navbarPage(
    title = app_title,
    windowTitle = app_title,
    position = "fixed-top",
    collapsible = TRUE,
    id = "tabs",
    
    # navbar tabs here
    tabPanel(
      title = "Tab1", value = "tab1", icon = icon("users"), 
      mod_ui("id")
    )
  ),
  
  waiter_preloader(html = spin_3())
)

# ==================================================
# APP SERVER

server <- function(input, output, session) {
  
  # error page
  sever(html = disconnected, bg_color = "white", color = "black")

  # user logs
  track_usage(
    storage_mode = store_json(path = here::here("logs")),
    app_name = app_name,
    exclude_users = c("paul.campbell@epicentre.msf.org", "paul")
  )

  # app guide info
  guide <- Cicerone$
    new()$
    step(
    "guide",
    "Guide",
    "Click this guide button at any time for a guided tour of the dashboard.",
    close_btn_text = "OK!",
    position = "bottom-center"
  )
  guide$init()$start()
  
  # server modules 
  mod_server("id")
  
}

# ==================================================
# LAUNCH APPLICATION 
shinyApp(ui, server)

