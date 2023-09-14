
delay_choices <- function(max_shipments, max_rounds) {
  shipments <- purrr::set_names(glue::glue("shipment_{1:max_shipments}"), glue::glue("Shipment {1:max_shipments}"))
  rounds <- purrr::set_names(glue::glue("round_{1:max_rounds}"), glue::glue("Round {1:max_rounds}"))
  c("Request" = "request_1", "Decision" = "decision_1", "Shipments" = list(shipments), "Rounds" = list(rounds))
}

group_vars <- c("Mechanism" = "r_mechanism", "Status" = "r_status")

dose_vars <- c("Requested" = "r_dose_request", "Approved" = "r_dose_approve", "Shipped" = "s_dose_ship")

date_vars <- c(
  "Date of request" = "r_date_receipt",
  "Date of decision" = "r_date_decision",
  "Date of delivery" = "s_date_delivery"
)

grouping_levels <- c("GTFCC", "ICG", "Loan", "Approved", "Not approved", "Pending", "Cancelled")

set_pal <- function(df, var) {
  pal <- c(
    "GTFCC" = "#1f77b4",
    "ICG" = "#ff7f0e",
    "Loan" = "#2ca02c",
    "Approved" = "#2ca02c",
    "Not approved" = "#d62728",
    "Pending" = "#1f77b4",
    "Cancelled" = "#ff7f0e",
    "Unknown" = "#7f7f7f"
  )
  levels_in_data <- levels(droplevels(df[[var]]))
  unname(pal[names(pal) %in% levels_in_data])
}

map_pal <- function(df) {
  pal <- c(
    "GTFCC" = "#1f77b4",
    "ICG" = "#ff7f0e",
    "Loan" = "#2ca02c",
    "Approved" = "#2ca02c",
    "Not approved" = "#d62728",
    "Pending" = "#1f77b4",
    "Cancelled" = "#ff7f0e",
    "Unknown" = "#7f7f7f"
  )
  levels_in_data <- colnames(df)
  unname(pal[names(pal) %in% levels_in_data])
}