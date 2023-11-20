
filter_geo <- function(df, geo_select) {
  # if (geo_select == "World") return(df)
  who_regions <- c("AMRO", "AFRO", "SEARO", "EMRO", "WPRO", "EURO")
  region_select <- geo_select[geo_select %in% who_regions]
  country_select <- setdiff(geo_select, who_regions)
  rl <- length(region_select)
  cl <- length(country_select)
  if (rl & cl) {
    df %<>% filter(r_who_region %in% region_select | r_country %in% country_select)
  } else if (rl) {
    df %<>% filter(r_who_region %in% region_select)
  } else if (cl) {
    df %<>% filter(r_country %in% country_select)
  }
  return(df)
}

get_q_range <- function(x) {
  q_range <- range(x, na.rm = TRUE) %>% 
    floor_date("quarter") %>% 
    as_date()
  q_range <- seq.Date(q_range[1], q_range[2], by = "3 months") %>% 
    quarter(with_year = TRUE) %>% 
    str_replace("\\.", "-Q")
  q_range
}

quarter_to_date <- function(quarter) {
  quarter %>% 
    stringr::str_replace("Q1", "01-01") %>% 
    stringr::str_replace("Q2", "04-01") %>% 
    stringr::str_replace("Q3", "07-01") %>% 
    stringr::str_replace("Q4", "10-01") %>% 
    lubridate::as_date()
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
      r_demand_id,
      content = glue::glue(
        "<b>Request</b>", 
        "{fmt_n_dose(r_dose_request)} doses requested", 
        "by {r_agency} for {r_country}",
        .sep = "</br>"
      ),
      start = r_date_receipt
    )
  
  decision <- df_request %>% 
    mutate(r_dose_approve = na_if(r_dose_approve, 0)) %>% 
    transmute(
      event = "decision",
      r_demand_id,
      content = glue::glue(
        "<b>Decision</b>", 
        "{fmt_n_dose(coalesce(r_dose_approve, r_dose_request))} doses {fmt_status(r_status)} via {r_mechanism}",
        "Decision took {replace_na(as.character(r_date_decision - r_date_receipt), '(unknown)')} days",
        .sep = "</br>"
      ),
      # start = coalesce(date_decision, date_receipt)
      start = r_date_decision
    )
  
  shipment <- df_shipment %>% 
    left_join(
      select(df_request, r_demand_id, r_date_decision),
      by = c("s_r_demand_id" = "r_demand_id")
    ) %>% 
    mutate(
      content = glue::glue(
        "<b>Delivery</b>", 
        "{fmt_n_dose(s_dose_ship)} doses of {s_vaccine}",
        "{replace_na(as.character(s_date_delivery - r_date_decision), '(unknown)')} days since request accepted",
        .sep = "</br>"
      ), 
      event = "shipment"
    ) %>% 
    select(event, r_demand_id = s_r_demand_id, content, start = s_date_delivery)  
  
  id_lookup <- distinct(df_shipment, r_demand_id = s_r_demand_id, c_s_id = s_id)
  
  round <- df_round %>% 
    filter(!is.na(cr_number)) %>% 
    mutate(
      event = "round",
      cr = str_sub(cr_id, start = -7) %>% replace_na("(Unknown)"),
      content = glue::glue("<b>Round {cr}</b></br>{fmt_n_dose(cr_dose_adm)} doses")
    ) %>% 
    left_join(id_lookup,by = "c_s_id") %>% 
    select(event, r_demand_id, content, start = cr_date_round_start)
  
  bind_rows(request, decision, shipment, round)
}


get_delay_df <- function(df_timeline) {
  df_timeline %>% 
    arrange(r_demand_id, start) %>% 
    group_by(r_demand_id, event) %>% 
    mutate(event = paste(event, row_number(), sep = "_")) %>% 
    ungroup() %>% 
    select(r_demand_id, event, date = start) 
}


# Map-Chart functions -----------------------------------------------------

#function to prepare the data

df_hc_bar <- function(df_data, 
                      request_dose, 
                      group_var, 
                      dose_type ) {
  
  if(request_dose == "Doses") {
    
    df <- df_data %>% 
      
      group_by(r_country, group = {{group_var}}) %>% 
      
      summarise(n = sum( {{dose_type}}, na.rm = TRUE) ) %>% 
      
      group_by(r_country) %>% 
      
      mutate(tot = sum(n, na.rm = TRUE)) %>% 
      
      ungroup() %>% 
      
      mutate( index = as.numeric(fct_reorder(r_country, desc(tot)) ), 
              
              #create a label for the total to be displayed in stacklabels
              label = if_else(tot >= 1000 & tot <= 99999, paste0(round(tot/1000, digits = 1), "K"),
                              
                              if_else(tot >=1000000, paste0(round(tot/1000000, digits = 1), "M"), 
                                      
                                      as.character(tot) ))
              
      )
    
  } else if(request_dose == "Requests") {
    
    df <- df_data %>% 
      
      count(r_country, 
            group = {{group_var}}) %>% 
      
      group_by(r_country) %>% 
      
      mutate(tot = sum(n, na.rm = TRUE)) %>% 
      
      ungroup() %>% 
      
      mutate(index = as.numeric(fct_reorder(r_country, desc(tot))),
             
             #create a label for the total to be displayed in stacklabels
             label = if_else(tot >= 1000 & tot <= 99999, paste0(round(tot/1000, digits = 1), "K"),
                             
                             if_else(tot >=1000000, paste0(round(tot/1000000, digits = 1), "M"), 
                                     
                                     as.character(tot) ))
      )
  }
  
  df <- df %>%  mutate(group = factor(group, grouping_levels))
  
  return(df)
} 

#function to make the map chart barplot   

hc_bar <- function(hc_bar_dat, request_dose, col_vec) {
  
  hchart(hc_bar_dat, 
         
         "column", 
         
         hcaes(index, 
               n, 
               group = group, 
               name = r_country)) %>%
    
    hc_chart(zoomType = "x") %>%
    
    hc_title(text = NULL) %>% 
    
    hc_xAxis(
      title = list(text = "Countries"),
      type = "category",
      crosshair = TRUE
    ) %>%
    
    hc_yAxis(title = list(text = paste0("Number of ", request_dose)), 
             allowDecimals = FALSE, 
             stackLabels = list(enabled = TRUE, 
                                align = "center", 
                                formatter = JS( "function() { 
                                                if(this.total <= 999){ 
                                                return this.total 
                                                } else if ( this.total >= 1000 &  this.total <= 999999) { return Math.round( this.total/1000) + ' K'
                                                } else { return Math.round( this.total/1000000) + ' M' }
                                                }" ) )
    ) %>%
    
    hc_plotOptions(column = list(stacking = "normal")) %>%
    
    hc_tooltip(
      
      shared = TRUE,
      
      pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y} ({point.percentage:.1f}%)</b><br/>'
      
    ) %>%
    
    hc_legend(
      title = list(text = ""),
      layout = "vertical",
      align = "right",
      verticalAlign = "top",
      x = -10,
      y = 40
    ) %>% 
    
    hc_colors(col_vec) %>% 
    
    my_hc_export( )
}