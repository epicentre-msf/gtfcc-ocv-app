
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

delay_vars <- tibble::tribble(
  ~lab, ~var, ~range, ~expected_days, 
  "Request -> Decision", "r_d", c("request_1", "decision_1"), 3,
  "Decision -> 1st Shipment", "d_s",  c("decision_1", "shipment_1"), 7,
  "1st Shipment -> Round 1", "s_r1",  c("shipment_1", "round_1"), 10,
  "Round 1 -> Round 2", "r1_r2",  c("round_1", "round_2"), NA_real_,
  "Request -> 1st Shipment", "r_s",  c("request_1", "shipment_1"), 10,
  "Request -> Round 1", "r_r1",  c("request_1", "round_1"), 20
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