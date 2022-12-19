ui <- tagList(
  tags$head(
    tags$link(href = google_font(app_font), rel = "stylesheet"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "addNavLink.js"),
    shinyjs::useShinyjs(),
    shinyWidgets::useShinydashboard(),
    pushbar::pushbar_deps(),
    waiter::use_waiter(),
    sever::useSever(),
    cicerone::use_cicerone()
  ),
  
  navbarPage(
    title = tagList(app_title, tags$small(glue::glue("last update: {date_updated}"))),
    windowTitle = app_title,
    position = "fixed-top",
    collapsible = TRUE,
    id = "tabs",
    
    # navbar tabs here
    tabPanel(
      title = "Requests", value = "request", icon = icon("list"),
      mod_request_ui("request")
    ),
    tabPanel(
      title = "Timeline", value = "timevis", icon = icon("chart-column"),
      mod_timevis_ui("timevis")
    )
  ),
  waiter_preloader(html = spin_3())
)
