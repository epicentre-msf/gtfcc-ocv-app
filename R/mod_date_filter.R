
mod_date_filter_ui <- function(id, request_range, shipment_range, round_range) {
  ns <- NS(id)
  tagList(
    shiny::helpText("Click the switch to activate each period filter."),
    tags$br(),
    date_filter(
      id = ns("request"),
      label = "Request period",
      range = request_range
    ),
    date_filter(
      id = ns("shipment"),
      label = "Shipment period",
      range = shipment_range
    ),
    date_filter(
      id = ns("round"),
      label = "Campaign period",
      range = round_range
    )
  )
}

mod_date_filter_server <- function(
    id,
    df_request,
    df_shipment,
    df_round,
    request_date = "r_date_receipt",
    shipment_date = "s_date_delivery",
    round_date = "cr_date_round_start"
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    id_lookup <- dplyr::distinct(df_shipment, r_demand_id = s_r_demand_id, c_s_id = s_id)

    df_filtered <- reactive({
      df_out <- df_request
      if (isTruthy(input$request_active)) {
        df_out <- df_request %>%
          dplyr::filter(dplyr::between(.data[[request_date]], input$request[1], input$request[2]))
      }
      if (isTruthy(input$shipment_active)) {
        shipment_filter <- df_shipment %>%
          dplyr::filter(dplyr::between(.data[[shipment_date]], input$shipment[1], input$shipment[2])) %>%
          dplyr::distinct(r_demand_id = s_r_demand_id)
        df_out <- df_out %>% dplyr::semi_join(shipment_filter, by = "r_demand_id")
      }
      if (isTruthy(input$round_active)) {
        round_filter <- df_round %>%
          dplyr::filter(dplyr::between(.data[[round_date]], input$round[1], input$round[2])) %>%
          dplyr::distinct(c_s_id) %>%
          dplyr::left_join(id_lookup, by = "c_s_id")
        df_out <- df_out %>% dplyr::semi_join(round_filter, by = "r_demand_id")
      }
      df_out
    })

    return(reactive(list(
      inputs = reactiveValuesToList(input),
      df = df_filtered()
    )))
  })
}

date_filter <- function(id, label, range) {
  htmltools::div(
    class = "d-flex",
    htmltools::div(
      class = "p-0 flex-grow-1", 
      shiny::dateRangeInput(
        inputId = id,
        label = label,
        start = range[1],
        end = range[2],
        min = range[1],
        max = range[2],
        format = "dd/mm/yy",
        startview = "decade",
        weekstart = 1,
        separator = "to"
      )
    ),
    htmltools::div(
      class = "p-0",
      bslib::input_switch(
        id = paste0(id, "_active"),
        label = NULL,
        value = FALSE
      )
    )
  )
}