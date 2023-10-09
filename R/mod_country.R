
mod_country_profile_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shinyWidgets::pickerInput(
        inputId = ns("country"),
        label = "Country",
        choices = na.omit(""),
        # options = picker_opts(search = TRUE),
        multiple = TRUE
      ),
      shinyWidgets::sliderTextInput(
        inputId = ns("time_period"),
        label = "Time period",
        choices = "",
        # selected = c(min(q_range), max(q_range)),
        grid = FALSE,
        animate = FALSE,
        width = "100%"
      ),
    ),
    bslib::layout_column_wrap(
      width = 1/5,
      fill = FALSE,
      bslib::value_box(
        title = "Requests approved", 
        value = textOutput(ns("n_approved")),
        textOutput(ns("approved_info")),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Campaigns", 
        value = textOutput(ns("n_campaigns")),
        textOutput(ns("campaigns_info")),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Doses", 
        value = textOutput(ns("n_doses")),
        textOutput(ns("doses_info")),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Targeted areas", 
        value = textOutput(ns("n_areas")),
        textOutput(ns("areas_info")),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Latest Campaign", 
        value = textOutput(ns("latest_campaign")),
        textOutput(ns("latest_campaign_info")),
        theme = "primary"
      )
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
          class = "d-flex justify-content-start align-items-center",
          "Bar chart"
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
        
        filter(country_name == input$country,
               
               between(d1_date_start, as.Date(input$time_period[1]), as.Date(input$time_period[2]) )
        ) })
    
    #summarise all data for the country_df
    country_summ <- reactive({ get_country_summ(country_df()) })
    
    #summarise the campaigns for the country df
    camp_summ <-  reactive ({ get_camp_summ(country_df()) })
    
    #get unique admin targeted for the country_df
    unique_admin <- reactive({ get_unique_admin(country_df()) } )
    
    # VALUE BOXES ==============================
    output$n_approved <- renderText({
      
    })
    output$approved_info <- renderText({
      
    })
    
    output$n_campaigns <- renderText({
      
    })
    output$campaigns_info <- renderText({
      
    })
    
    output$n_doses <- renderText({
      
    })
    output$doses_info <- renderText({
      
    })
    
    output$n_areas <- renderText({
      
    })
    output$areas_info <- renderText({
      
    })
    
    output$latest_campaign <- renderText({
      
    })
    output$latest_campaign_info <- renderText({
      
    })
    
    # GRAPHICS/TABLES ==========================
    output$timevis <- timevis::renderTimevis({
      
    })
    
    output$chart <- highcharter::renderHighchart({
      
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
      d1_date_start = t_d1_date_round_start,
      d1_date_end = t_d1_date_round_end,
      d1_dose_adm  = t_d1_dose_adm,
      d1_cov_adm = t_d1_cov_adm,
      d2_date_start = t_d2_date_round_start,
      d2_date_end  = t_d2_date_round_end,
      d2_dose_adm = t_d2_dose_adm,
      d2_cov_adm = t_d2_cov_adm,
      d2_drop_n = t_d2_drop_n,
      d2_drop_p  = t_d2_drop_p,
      comments = t_comments
    ) %>% 
    
    group_by(country_code) %>% 
    
    mutate(camp_id = match(request_id, unique(request_id)), 
           camp_id = paste0("campaign_", camp_id)) %>% 
    
    ungroup() %>% 
    
    relocate(camp_id, .after = country_code) %>% 
    
    #add a variable saying if there is one or two dose for the campaign 
    
    mutate(n_rounds = if_else(if_any(c(d1_date_start, d2_date_start), ~ is.na(.x)), "one dose", "two doses" )) %>% 
    
    #aggregate to admin3 level 
    group_by(country_code, 
             country_name,
             camp_id, 
             d1_date_start, 
             d1_date_end, 
             d2_date_start, 
             d2_date_end,
             campaign_type,
             campaign_strategy,
             n_rounds,
             target_type,
             adm1_name, 
             adm2_name, 
             adm3_name) %>% 
    
    summarise(across(is.numeric, ~ sum(.x)) ) %>% 
    
    ungroup() %>% 
    
    mutate(
      d1_cov_adm = d1_dose_adm/target_pop,
      d2_cov_adm = d2_dose_adm/target_pop 
      
    )
}


# function that summarize the campaigns done by a country
get_camp_summ <- function(country_df) {
  
  camp_sum <- country_df %>% 
    
    group_by(country_name, camp_id) %>% 
    
    summarise(
      d1_date_start = min(d1_date_start), 
      d1_date_end = max(d1_date_end),
      d2_date_start = min(d2_date_start), 
      d2_date_end = max(d2_date_end),
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
      cov_d1 = n_d1/target_pop, 
      n_d2 = sum(d2_dose_adm), 
      cov_d2 = n_d2/target_pop)
  
  return(camp_sum)
}

# function that summarize all data for each country using the summary for campaigns
get_country_summ <- function(camp_sum_df) {
  
  coun_sum <- camp_sum_df %>% 
    
    group_by(country_name) %>% 
    
    summarise(
      n_campaigns = n(),
      n_reactive = sum(campaign_type == "Reactive"), 
      pct_reactive = round(digits = 0, n_reactive/n_campaign * 100),
      n_preventive = sum(campaign_type == "Preventive"),
      pct_preventive = round(digits = 0, n_preventive/n_campaign * 100),
      target_pop = sum(target_pop),
      n_d1 = sum(n_d1), 
      cov_d1 = n_d1/target_pop, 
      n_d2 = sum(n_d2), 
      cov_d2 = n_d2/target_pop)
  
  return(coun_sum)
}


# Function to get unique number of admin area targetted per country 
get_unique_admin <- function(country_df) {
  
  country_df %>% 
    
    summarise(unique_adm1 = n_distinct(adm1_name), 
              unique_adm2 = n_distinct(adm2_name), 
              unique_adm3 = n_distinct(adm3_name)
    ) }

