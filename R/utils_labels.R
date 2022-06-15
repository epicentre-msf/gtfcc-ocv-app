
delay_choices <- function(max_shipments, max_rounds) {
  shipments <- purrr::set_names(glue::glue("shipment_{1:max_shipments}"), glue::glue("Shipment {1:max_shipments}"))
  rounds <- purrr::set_names(glue::glue("round_{1:max_rounds}"), glue::glue("Round {1:max_rounds}"))
  c("Request" = "request_1", "Decision" = "decision_1", "Shipments" = list(shipments), "Rounds" = list(rounds))
}
