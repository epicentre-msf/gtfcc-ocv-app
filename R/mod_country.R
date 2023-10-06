
mod_country_profile_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shinyWidgets::pickerInput(
        inputId = ns("country"),
        label = "Country",
        choices = na.omit(""),
        # options = picker_opts(search = TRUE),
        multiple = TRUE
      ),
      shinyWidgets::sliderTextInput(
        inputId = ns("time_period"),
        label = "Time period",
        choices = "",
        # selected = c(min(q_range), max(q_range)),
        grid = FALSE,
        animate = FALSE,
        width = "100%"
      ),
    ),
    bslib::layout_column_wrap(
      width = 1/5,
      fill = FALSE,
      value_box(
        title = "Requests approved", 
        value = textOutput(ns("n_approved")),
        textOutput(ns("approved_info")),
        theme = "primary"
      ),
      value_box(
        title = "Campaigns", 
        value = textOutput(ns("n_campaigns")),
        textOutput(ns("campaigns_info")),
        theme = "primary"
      ),
      value_box(
        title = "Doses", 
        value = textOutput(ns("n_doses")),
        textOutput(ns("doses_info")),
        theme = "primary"
      ),
      value_box(
        title = "Targeted areas", 
        value = textOutput(ns("n_areas")),
        textOutput(ns("areas_info")),
        theme = "primary"
      ),
      value_box(
        title = "Latest Campaign", 
        value = textOutput(ns("latest_campaign")),
        textOutput(ns("latest_campaign_info")),
        theme = "primary"
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6, 6, 6),
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-start align-items-center",
          "Timeline"
        ),
        bslib::card_body(timevis::timevisOutput(ns("timevis")))
      ),
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-start align-items-center",
          "Bar chart"
        ),
        bslib::card_body(highcharter::highchartOutput(ns("chart")))
      ),
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-start align-items-center",
          "Map"
        ),
        bslib::card_body(leaflet::leafletOutput(ns("map")))
      ),
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-start align-items-center",
          "Summary table"
        ),
        bslib::card_body(reactable::reactableOutput(ns("tbl")))
      )
    )
  )
}

mod_country_profile_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # OBSERVERS ================================

    # PREPARE DATA =============================

    # VALUE BOXES ==============================
    output$n_approved <- renderText({

    })
    output$approved_info <- renderText({

    })

    output$n_campaigns <- renderText({

    })
    output$campaigns_info <- renderText({

    })

    output$n_doses <- renderText({

    })
    output$doses_info <- renderText({

    })

    output$n_areas <- renderText({

    })
    output$areas_info <- renderText({

    })

    output$latest_campaign <- renderText({

    })
    output$latest_campaign_info <- renderText({

    })

    # GRAPHICS/TABLES ==========================
    output$timevis <- timevis::renderTimevis({

    })

    output$chart <- highcharter::renderHighchart({

    })

    output$map <- leaflet::renderLeaflet({

    })

    output$tbl <- reactable::renderReactable({

    })


  })
}
