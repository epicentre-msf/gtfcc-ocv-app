
delay_choices <- function(max_shipments, max_rounds) {
  shipments <- purrr::set_names(glue::glue("shipment_{1:max_shipments}"), glue::glue("Shipment {1:max_shipments}"))
  rounds <- purrr::set_names(glue::glue("round_{1:max_rounds}"), glue::glue("Round {1:max_rounds}"))
  c("Request" = "request_1", "Decision" = "decision_1", "Shipments" = list(shipments), "Rounds" = list(rounds))
}

group_vars <- c("Mechanism" = "request_mechanism", "Status" = "request_status")

dose_vars <- c("Requested" = "n_dose_request", "Approved" = "n_dose_approve", "Shipped" = "n_dose_ship")

date_vars <- c(
  "Date of receipt" = "date_receipt",
  "Date of decision" = "date_decision",
  "Date of delivery" = "date_delivery"
)

grouping_levels <- c("GTFCC", "ICG", "Loan", "Approved", "Not approved", "Pending", "Cancelled")