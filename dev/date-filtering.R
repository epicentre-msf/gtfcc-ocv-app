library(shiny)
library(bslib)
library(tidyverse)
source(here::here("R", "mod_date_filter.R"))

app_data <- readr::read_rds(here::here("data", "app_data.rds"))
df_request <- app_data$request
df_round <- app_data$campaign_and_round
df_shipment <- app_data$shipment

request_range <- range(df_request$r_date_receipt, na.rm = TRUE)
shipment_range <- range(df_shipment$s_date_delivery, na.rm = TRUE)
round_range <- range(df_round$cr_date_round_start, na.rm = TRUE)
# to join request ids to round data
id_lookup <- distinct(df_shipment, r_demand_id = s_r_demand_id, c_s_id = s_id)

ui <- page_sidebar(
    title = "Date Filtering",
    sidebar = sidebar(
      mod_date_filter_ui(
        id = "df",
        request_range,
        shipment_range,
        round_range
      )
    ),
    # bslib::layout_column_wrap(
    #   fillable = FALSE,
    #   bslib::value_box(
    #     title = "Selected requests",
    #     value = textOutput("n_requests")
    #   )
    # ),
    bslib::layout_column_wrap(
      reactable::reactableOutput("tbl")
    )
)

server <- function(input, output, session) {

  # output$n_requests <- renderText({
  #   length(
  #     unique(
  #       c(
  #         request_period_filter(),
  #         shipment_period_filter(),
  #         round_period_filter()
  #       )
  #     )
  #   )
  # })

  df_mod <- mod_date_filter_server(
    id = "df",
    df_request,
    df_shipment,
    df_round
  )

  output$tbl <- reactable::renderReactable({
    reactable::reactable(df_mod())
  })

  # observe({
  #   print(request_period_filter())
  #   print(shipment_period_filter())
  #   print(round_period_filter())
  # })
}

shinyApp(ui, server)