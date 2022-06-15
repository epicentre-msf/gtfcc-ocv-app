
filter_geo <- function(df, geo_select) {
  # if (geo_select == "World") return(df)
  who_regions <- c("AMRO", "AFRO", "SEARO", "EMRO", "WPRO", "EURO")
  region_select <- geo_select[geo_select %in% who_regions]
  country_select <- setdiff(geo_select, who_regions)
  rl <- length(region_select)
  cl <- length(country_select)
  if (rl & cl) {
    df %<>% filter(who_region %in% region_select | request_country %in% country_select)
  } else if (rl) {
    df %<>% filter(who_region %in% region_select)
  } else if (cl) {
    df %<>% filter(request_country %in% country_select)
  }
  return(df)
}

fmt_n_dose <- function(n) {
  if (is.na(n)) {
    "(Unknown)"
  } else if (n < 1000) {
    n
  } else {
    scales::number(
      n,
      accuracy = .1,
      scale_cut = c(0, K = 1e3, M = 1e6)
    ) %>% str_remove("\\.0")
  }
}
fmt_n_dose <- Vectorize(fmt_n_dose)

fmt_status <- function(x) {
  str_to_lower(x) %>% str_replace("not", "<b>not</b>")
}

get_timevis_df <- function(df_request, df_shipment, df_round) {
  request <- df_request %>% 
    transmute(
      event = "request",
      id_demand,
      content = glue::glue(
        "<b>Request</b>", 
        "{fmt_n_dose(n_dose_request)} doses requested", 
        "by {request_agency} for {request_country}",
        .sep = "</br>"
      ),
      start = date_receipt
    )
  
  decision <- df_request %>% 
    mutate(n_dose_approve = na_if(n_dose_approve, 0)) %>% 
    transmute(
      event = "decision",
      id_demand,
      content = glue::glue(
        "<b>Decision</b>", 
        "{fmt_n_dose(coalesce(n_dose_approve, n_dose_request))} doses {fmt_status(request_status)} via {request_mechanism}",
        "Decision took {replace_na((date_decision - date_receipt), '(unknown)')} days",
        .sep = "</br>"
      ),
      # start = coalesce(date_decision, date_receipt)
      start = date_decision
    )
  
  shipment <- df_shipment %>% 
    mutate(
      content = glue::glue(
        "<b>Delivery</b>", 
        "{fmt_n_dose(n_dose_ship)} doses of {vaccine}",
        "Shipment took {replace_na((date_delivery - date_ship), '(unknown)')} days",
        .sep = "</br>"
      ), 
      event = "shipment"
    ) %>% 
    select(event, id_demand, content, start = date_delivery)  
  
  round <- df_round %>% 
    filter(!is.na(round_number)) %>% 
    mutate(
      event = "round",
      cr = str_sub(id_round, start = -5) %>% replace_na("(Unknown)"),
      content = glue::glue("<b>Round {cr}</b></br>{fmt_n_dose(n_dose_admin)} doses")
    ) %>% 
    select(event, id_demand, content, start = date_round)
  
  bind_rows(request, decision, shipment, round)
}


get_delay_df <- function(df_timeline) {
  df_timeline %>% 
    arrange(id_demand, start) %>% 
    group_by(id_demand, event) %>% 
    mutate(event = paste(event, row_number(), sep = "_")) %>% 
    ungroup() %>% 
    select(id_demand, event, date = start) 
}
