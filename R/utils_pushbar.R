pushbar_ui <- function(ns) {
  pushbar::pushbar(
    id = ns("filters"),
    style = "background:#fbfbfb; margin-top: 50px;",
    div(
      class = "sidebar-inputs",
      shinyWidgets::sliderTextInput(
        inputId = ns("q_range"),
        label = "Quarter",
        choices = q_range,
        selected = c(min(q_range), max(q_range)),
        grid = FALSE,
        animate = FALSE,
        width = "95%"
      ),
      div(
        id = ns("resetable_filters"),
        # treeviewInput(
        #   inputId = ns("geo"),
        #   label = "Select a region/country:",
        #   # choices = do.call(c, list(list(list(id = "world", text = "World")), make_tree(df_request, c("who_region", "request_country")))),
        #   choices = make_tree(df_request, c("who_region", "request_country")),
        #   selected = "world",
        #   multiple = TRUE,
        #   prevent_unselect = FALSE,
        #   width = "100%"
        # ),
        shinyWidgets::pickerInput(
          inputId = ns("region"),
          label = "Region",
          choices = unique(df_request$r_who_region) %>% na.omit(),
          options = picker_opts(),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = ns("country"),
          label = "Country",
          choices = unique(df_request$r_country) %>% na.omit(),
          options = picker_opts(search = TRUE),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = ns("status"),
          label = "Status",
          choices = unique(df_request$r_status) %>% na.omit(),
          options = picker_opts(),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = ns("context"),
          label = "Context",
          choices = unique(df_request$r_context) %>% na.omit(),
          options = picker_opts(),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = ns("mechanism"),
          label = "Mechanism",
          choices = unique(df_request$r_mechanism) %>% na.omit(),
          options = picker_opts(),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = ns("agency"),
          label = "Request Agency",
          choices = unique(df_request$r_agency) %>% na.omit(),
          options = picker_opts(),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = ns("vaccine"),
          label = "Type of vaccine",
          choices = unique(df_shipment$s_vaccine) %>% na.omit(),
          options = picker_opts(),
          multiple = TRUE
        ),
        actionButton(ns("go2"), "Update Data", icon = icon("database"), class = "btn-primary btn-sm", style = "color: #fff;"),
        actionButton(ns("reset"), "Reset Inputs", icon = icon("arrows-rotate"), class = "btn-info btn-sm", style = "color: #fff;"),
        actionButton(ns("close"), "Close", icon = icon("xmark"), class = "btn-danger btn-sm", style = "color: #fff;")
      )
    )
  )
}
