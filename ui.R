ui <- tagList(
  tags$head(
    tags$style(".value-box-area {padding: 5px !important;}"),
    # tags$link(href = google_font(p_font), rel = "stylesheet"),
    tags$style(
      HTML(glue::glue("p {{font-family: '{app_font}';}}"))
    ),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    sever::useSever()
  ),
  
  page_navbar(
    title = tagList(
      app_title,
      tags$small(
        glue::glue("data updated: {date_updated}"),
        class = "text-muted",
        style = "font-size: 0.6em;"
      )
    ),
    id = "tabs",
    window_title = app_title,
    fillable = TRUE,
    collapsible = TRUE,
    inverse = FALSE,
    padding = 0,
    theme = bs_theme(
      base_font = font_google(
        app_font, 
        wght = c(300, 400, 500, 600, 700, 800),
        ital = c(0, 1)
      ),
      success = SUCCESS,
      warning = WARNING,
      font_scale = 0.8,
      bootswatch = "litera"
    ),
    
    # navbar tabs here
    nav_panel(
      title = "Requests", value = "request", icon = icon("list"),
      mod_request_ui("request")
    ),
    nav_panel(
      title = "Timeline", value = "timevis", icon = icon("chart-column"),
      mod_timevis_ui("timevis")
    ),
    nav_panel(
      title = "Country Profile", value = "country", icon = icon("globe-africa"),
      mod_country_profile_ui("country")
    ),
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
      tags$a(
        tags$img(
          src = "gtfcc-logo.jpg",
          height = "35px"
        ),
        class = "p-0",
        href = "https://www.gtfcc.org/",
        target = "_blank"
      )
    )
  ),
  waiter_preloader(html = spin_3())
)
