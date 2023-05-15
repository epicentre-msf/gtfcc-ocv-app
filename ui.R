ui <- tagList(
  tags$head(
    # tags$style(
    #   HTML("body {margin-top: 60px;}")
    # ),
    # tags$link(href = google_font(app_font), rel = "stylesheet"),
    # tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # tags$script(src = "addNavLink.js"),
    # shinyWidgets::useShinydashboard(),
    # pushbar::pushbar_deps(),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    sever::useSever()
    # cicerone::use_cicerone()
  ),
  
  page_navbar(
    title = tagList(app_title, tags$small(glue::glue("last update: {date_updated}"))),
    id = "tabs",
    window_title = app_title,
    # position = "fixed-top",
    fillable = TRUE,
    collapsible = TRUE,
    # bg = "#f8f9fa",
    inverse = FALSE,
    theme = bs_theme(
      base_font = font_google(
        app_font, 
        wght = c(300, 400, 500, 600, 700, 800),
        ital = c(0, 1)
      ),
      bootswatch = "litera",
      # "navbar-bg" = "#f8f9fa",
      # "navbar-light-bg" = "#f8f9fa",
      # "navbar-dark-bg" = "#f8f9fa",
      # primary = "#2E569E",
      # secondary = "#D37331",
      # success = "#94BA3B"
    ),
    
    # navbar tabs here
    nav(
      title = "Requests", value = "request", icon = icon("list"),
      mod_request_ui("request")
    ),
    # nav(
    #   title = "Timeline", value = "timevis", icon = icon("chart-column"),
    #   mod_timevis_ui("timevis")
    # ),
    nav_spacer(),
    nav_item(
      tags$a(
        shiny::icon("github"),
        "Report an issue",
        href = "https://github.com/epicentre-msf/gtfcc-ocv-app/issues",
        target = "_blank"
      )
    ),
    nav_item(
      tags$img(
        src = "gtfcc-logo.jpg",
        height = "40px"
      )
    )
  ),
  waiter_preloader(html = spin_3())
)
