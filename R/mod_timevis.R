
mod_timevis_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "reactive-width",
    shinyWidgets::pickerInput(
      inputId = ns("demand"),
      label = "Demand",
      choices = "",
      options = picker_opts(search = TRUE),
      multiple = FALSE
    ),
    timevis::timevisOutput(ns("timevis"))
  )
}

mod_timevis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

    df_timevis <- get_timevis_df(df_request, df_shipment, df_round)
    
    observe({
      demands <- sort(unique(df_timevis$id_demand))
      shinyWidgets::updatePickerInput(session, "demand", choices = demands)
    })
    
    output$timevis <- renderTimevis({
      req(input$demand)
      
      df_out <- df_timevis %>% 
        filter(id_demand == input$demand) %>% 
        drop_na(start)
      
      dates <- unique(df_out$start)
      
      start_date <- min(dates) - 3
      end_date <- max(dates) + 3
      
      # if (length(dates) == 1) {
      #   start_date <- dates - 3
      #   end_date <- dates + 3
      # } else {
      #   start_date <- min(dates) - 1
      #   end_date <- max(dates) + 1
      # }
      
      timevis(
        df_out,
        options = list(
          start = start_date, 
          end = end_date
          # timeAxis = list(scale = "day")
        )
      )
    })
    
    
  })
}
