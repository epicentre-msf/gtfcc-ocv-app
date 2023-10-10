
mod_country_profile_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shinyWidgets::pickerInput(
        inputId = ns("country"),
        label = "Country",
        choices = c("Cameroon" = "CMR"), #na.omit(""),
        # options = picker_opts(search = TRUE)
      ),
      
      shiny::sliderInput(
        inputId = ns("time_period"),
        label = "Time period",
        min = as.Date("2021-07-03"), 
        max = as.Date("2023-02-01"),
        value = c( as.Date("2021-07-03") , as.Date("2023-02-01")),
        width = "100%"
        
      ) ), 
    
    bslib::layout_column_wrap(
      width = 1/3,
      fill = FALSE,
      #   bslib::value_box(
      #     title = "Requests approved", 
      #     value = uiOutput(ns("n_approved")),
      #     uiOutput(ns("approved_info")),
      #     theme = "primary"
      #   ),
      
      bslib::value_box(
        title = "Campaigns", 
        value = uiOutput(ns("n_campaigns")),
        uiOutput(ns("campaigns_info")),
        theme = "primary"
      ),
      
      bslib::value_box(
        title = "Doses", 
        value = uiOutput(ns("n_doses")),
        uiOutput(ns("doses_info")),
        theme = "primary"
      ),
      
      bslib::value_box(
        title = "Targeted areas", 
        value = uiOutput(ns("n_areas")),
        uiOutput(ns("areas_info")),
        theme = "primary"
      ),
      
      # bslib::value_box(
      #   title = "Latest Campaign", 
      #   value = uiOutput(ns("latest_campaign")),
      #   uiOutput(ns("latest_campaign_info")),
      #   theme = "primary"
      # )
    ),
    
    bslib::layout_columns(
      col_widths = c(6, 6, 6, 6),
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-start align-items-center",
          "Timeline"
        ),
        bslib::card_body(timevis::timevisOutput(ns("timevis")))
      ),
      
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          "Bar chart",
          
          div(class = "pe-1", 
              shinyWidgets::radioGroupButtons(
                ns("bar_unit"),
                label = NULL,
                choices = c("Doses counts" = "n", "Coverage" = "cov"),
                selected = "n",
                size = "sm",
                status = "outline-success"
              ))
        ),
        
        bslib::card_body(highcharter::highchartOutput(ns("chart")))
      ),
      
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-start align-items-center",
          "Map"
        ),
        bslib::card_body(leaflet::leafletOutput(ns("map")))
      ),
      
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-start align-items-center",
          "Summary table"
        ),
        
        bslib::card_body(reactable::reactableOutput(ns("tbl")))
      )
    )
  )
}

mod_country_profile_server <- function(id, df_country_profile) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # OBSERVERS ================================
    
    # PREPARE DATA =============================
    
    #prepare the data
    prep_dat <- prep_data(df_country_profile)
    
    #filter the data for the selected country and date range
    country_df <- reactive({ prep_dat %>% 
        
        filter(country_code == input$country,
               
               between(date_start_d1, as.Date(input$time_period[1]), as.Date(input$time_period[2]) )
        ) })
    
    #summarise the campaigns for the country df
    request_summ <-  reactive ({ get_request_summ(country_df()) })
    
    #get the latest campaign
    latest_camp <- reactive( { 
      
      request_summ() %>% 
        
        filter(d2_date_end == max(d2_date_end) ) })
    
    #summarise all data for the country_df
    #country_summ <- reactive({ get_country_summ(camp_summ()) })
    
    #summarise rounds
    rounds_summ <- reactive({ get_rounds_summ(request_summ())})
    
    #get unique admin targeted for the country_df
    unique_admin <- reactive({ get_unique_admin(country_df()) } )
    
    
    # VALUE BOXES ==============================
    # output$n_approved <- renderUI({
    # })
    
    # output$approved_info <- renderUI({
    #   
    # })
    
    output$n_campaigns <- renderUI({
      
      tagList( p(glue::glue("{nrow(request_summ())}"))
      )  
    })
    
    output$campaigns_info <- renderUI({
      
      tagList(p(glue::glue("{epikit::fmt_count(request_summ(), campaign_type == 'Reactive')} reactive campaigns")),
              p(glue::glue("{epikit::fmt_count(request_summ(), n_rounds == 'single dose') } single dose campaigns")))
      
    })
    
    output$n_doses <- renderUI({
      
      tagList( p( glue::glue(" {fmt_n_dose(sum(request_summ()$total_doses))} administered doses") ) )
      
    })
    
    output$doses_info <- renderUI({
      
      tagList( p(glue::glue("{fmt_n_dose(sum(get_request_sum(cmr)$n_d1))} first doses")) ) 
      
    })
    
    output$n_areas <- renderUI({
      
      tagList( p(glue::glue("{unique_admin()$unique_adm1} admin 1 levels")),
               p(glue::glue("{unique_admin()$unique_adm2} admin 2 levels")), 
               p(glue::glue("{unique_admin()$unique_adm3} admin 3 levels")))
      
    })
    
    output$areas_info <- renderUI({
      
      
    })
    
    output$latest_campaign <- renderUI({
      
      
      #tagList( p(glue::glue("from {latest_camp()$d1_date_start} to {latest_camp()$d2_date_end}")) )
      
    })
    
    output$latest_campaign_info <- renderUI({
      
      # tagList( 
      #   p(glue::glue("{latest_camp()$campaign_type} campaign")),
      #   p(glue::glue("targeting {latest_camp()$target_type }", " ({fmt_n_dose(latest_camp()$target_pop)} people)")),
      #   p(glue::glue("{latest_camp()$cov_d1}% first dose coverage")), 
      #   p(glue::glue("{latest_camp()$cov_d2}% second dose coverage")), 
      #   
      #   p(glue::glue("{latest_camp()$drop_out}% dropout rate"))
      # )
      
    })
    
    # GRAPHICS/TABLES ==========================
    output$timevis <- timevis::renderTimevis({
      
    })
    
    output$chart <- highcharter::renderHighchart({
      
      rounds_summ() %>% 
        
        mutate( 
          #datestart = as.character(datestart),
          campaign_type = str_to_lower(campaign_type), 
          target_pop = fmt_n_dose(target_pop), 
          total_doses = fmt_n_dose(total_doses), 
          cov_label = scales::percent(cov, scale = 1),
          n_label = fmt_n_dose(n),
          
          dose = case_match(dose, 
                            "d1" ~ 'Dose 1', 
                            "d2" ~ "Dose 2")) %>% 
        
        hchart(., 
               "column",
               
               hcaes(x = request_id, 
                     y = !!sym(input$bar_unit),
                     group = dose
               ) ) %>% 
        
        # hc_xAxis(min = "2020-01-01",
        #           max = "2023-12-12") %>% 
        # 
        hc_plotOptions(column = list( stacking = NULL) ) %>% 
        
        hc_tooltip(formatter = JS(
          "
    function(){ 
    outHTML =  '<b>' + this.point.request_id + '</b> - <i>' + this.point.dose +'<br>' + this.point.n_rounds + ' ' + 
    this.point.campaign_type + '</i> campaign' +  '<br>Target: ' + this.point.target_type + 
    '(' + this.point.target_pop + ')<br>' + 'Doses administered: ' + this.point.n_label + '<br> Coverage: ' + this.point.cov_label
    
    return(outHTML)
    }
    " ) ) %>% 
        
        my_hc_export()
      
    })
    
    output$map <- leaflet::renderLeaflet({
      
    })
    
    output$tbl <- reactable::renderReactable({
      
    })
    
  })
}


# Functions for module ===============================================

#function to prepare the data for country profile
prep_data <- function(target_area_df) {
  
  #make new id 
  target_area_df %>%
    
    select(
      request_id = t_r_id, 
      country_code, 
      country_name = t_r_country,
      adm1_name = adm1_t_target_area,
      adm2_name = adm2_t_target_area,
      adm3_name = adm3_t_target_area,
      adm4_name = adm4_t_target_area,
      campaign_type = t_campaign_type,
      campaign_strategy = t_campaign_strategy,
      target_area_pop = t_target_area_population,
      target_pop  = t_target_population,
      target_type = t_target_type,
      target_type_details = t_target_type_details,
      date_start_d1 = t_d1_date_round_start,
      date_end_d1 = t_d1_date_round_end,
      d1_dose_adm  = t_d1_dose_adm,
      d1_cov_adm = t_d1_cov_adm,
      date_start_d2 = t_d2_date_round_start,
      date_end_d2  = t_d2_date_round_end,
      d2_dose_adm = t_d2_dose_adm,
      d2_cov_adm = t_d2_cov_adm,
      d2_drop_n = t_d2_drop_n,
      d2_drop_p  = t_d2_drop_p,
      comments = t_comments
    ) %>% 
    
    #group_by(country_code) %>% 
    
    #mutate(camp_id = match(request_id, unique(request_id)), 
    #       camp_id = paste0("campaign_", camp_id)) %>% 
    
    #ungroup() %>% 
    
    #relocate(camp_id, .after = country_code) %>% 
    
    #add a variable saying if there is one or two dose for the campaign 
  
  mutate(n_rounds = if_else(if_any(c(date_start_d1, date_start_d2), ~ is.na(.x)), "one dose", "two doses" )) %>% 
    
    #aggregate to admin3 level 
    group_by(country_code, 
             country_name,
             #camp_id,
             request_id,
             date_start_d1, 
             date_end_d1, 
             date_start_d2, 
             date_end_d2,
             campaign_type,
             campaign_strategy,
             n_rounds,
             target_type,
             adm1_name, 
             adm2_name, 
             adm3_name) %>% 
    
    summarise(across(is.numeric, ~ sum(.x)), .groups = "drop" ) %>% 
    
    mutate(
      d1_cov_adm = d1_dose_adm/target_pop,
      d2_cov_adm = d2_dose_adm/target_pop 
      
    )
}

# function that summarize the requests by a country
get_request_summ <- function(country_df) {
  
  req_summ <- country_df %>% 
    
    group_by(country_name, request_id) %>% 
    
    summarise(
      date_start_d1 = min(date_start_d1), 
      date_end_d1 = max(date_end_d1),
      date_start_d2 = min(date_start_d2), 
      date_end_d2 = max(date_end_d2),
      n_rounds = unique(n_rounds),
      n_target_area = n(), 
      n_adm1 = n_distinct(adm1_name, na.rm = TRUE),
      n_adm2 = n_distinct(adm2_name, na.rm = TRUE), 
      n_adm3 = n_distinct(adm3_name, na.rm = TRUE),
      smallest_target = if_else(n_adm3 == 0, "adm2", if_else(n_adm2 == 0, "adm1", "adm3")), 
      campaign_type = unique(campaign_type),
      target_type = paste0(unique(target_type), collapse = ", "),
      target_pop = sum(target_pop),
      n_d1 = sum(d1_dose_adm), 
      cov_d1 = round(digits = 1, n_d1/target_pop * 100) , 
      n_d2 = sum(d2_dose_adm), 
      cov_d2 = round(digits = 1, n_d2/target_pop * 100), 
      .groups = "drop"
      
    ) %>% 
    
    mutate(total_doses = n_d1 + n_d2)
  
  return(req_summ)
}


#functions to get a summary of rounds for requests 
get_rounds_summ <- function(request_summ_df) {
  
  request_summ_df %>% 
    
    rename_with(.cols = c(date_start_d1, date_end_d1, date_start_d2, date_end_d2), ~ str_replace(.x, "_", "") ) %>% 
    
    pivot_longer(cols = c(n_d1, n_d2, cov_d1, cov_d2, datestart_d1, dateend_d1, datestart_d2, dateend_d2), 
                 names_sep = "_", 
                 names_to = c(".value", "dose"), 
                 #removes if doses values is NA 
                 values_drop_na = TRUE
    ) 
}

# 
# # function that summarize all data for each country using the summary for campaigns
# get_country_summ <- function(camp_sum_df) {
#   
#   coun_sum <- camp_sum_df %>% 
#     
#     group_by(country_name) %>% 
#     
#     summarise(
#       n_campaigns = n(),
#       n_reactive = sum(campaign_type == "Reactive"), 
#       pct_reactive = round(digits = 0, n_reactive/n_campaigns * 100),
#       n_preventive = sum(campaign_type == "Preventive"),
#       pct_preventive = round(digits = 0, n_preventive/n_campaigns * 100),
#       target_pop = sum(target_pop),
#       n_d1 = sum(n_d1), 
#       cov_d1 = n_d1/target_pop, 
#       n_d2 = sum(n_d2), 
#       cov_d2 = n_d2/target_pop)
#   
#   return(coun_sum)
# }

# Function to get unique number of admin area targetted per country 
get_unique_admin <- function(country_df) {
  
  country_df %>% 
    
    summarise(unique_adm1 = n_distinct(adm1_name), 
              unique_adm2 = n_distinct(adm2_name), 
              unique_adm3 = n_distinct(adm3_name)
    ) }