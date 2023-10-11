
mod_country_profile_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    fillable = FALSE,
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
      bslib::value_box(
        title = "Requests approved", 
        value = textOutput(ns("n_approved")),
        textOutput(ns("approved_info")),
        showcase = bsicons::bs_icon("box-seam"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Campaigns", 
        value = textOutput(ns("n_campaigns")),
        textOutput(ns("campaigns_info")),
        showcase = bsicons::bs_icon("box-seam"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Doses", 
        value = textOutput(ns("n_doses")),
        uiOutput(ns("doses_info")),
        showcase = bsicons::bs_icon("box-seam"),
        theme = "primary"
      )
      # bslib::value_box(
      #   title = "Targeted areas", 
      #   value = textOutput(ns("n_areas")),
      #   uiOutput(ns("areas_info")),
      #   theme = "primary"
      # ),
      # bslib::value_box(
      #   title = "Latest Campaign", 
      #   value = textOutput(ns("latest_campaign")),
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
        bslib::card_body(
          padding = 0,
          timevis::timevisOutput(ns("timevis"))
        )
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
  moduleServer(id, function(input,
                            output,
                            session) {
    ns <- session$ns
    
    # OBSERVERS ================================
    
    # PREPARE DATA =============================
    
    # prepare the data
    prep_dat <- prep_data(df_country_profile)
    
    # filter the data for the selected country and date range
    country_df <- reactive({
      prep_dat %>%
        dplyr::filter(
          country_code == input$country,
          between(d1_date_start, as.Date(input$time_period[1]), as.Date(input$time_period[2]))
        )
    })
    
    # summarise the campaigns for the country df
    camp_summ <-  reactive ({ get_camp_summ(country_df()) })
    
    # get the latest campaign
    latest_camp <- reactive({
      camp_summ() %>% dplyr::filter(d2_date_end == max(d2_date_end))
    })
    
    #summarise all data for the country_df
    country_summ <- reactive({ get_country_summ(camp_summ()) })
    
    #get unique admin targeted for the country_df
    unique_admin <- reactive({ get_unique_admin(country_df()) })
    
    
    # VALUE BOXES ==============================
    output$n_approved <- renderText({
      
    })
    
    output$approved_info <- renderText({
      
    })
    
    output$n_campaigns <- renderText({
      scales::number(country_summ()$n_campaigns)
    })
    
    output$campaigns_info <- renderText({
      glue::glue("{country_summ()$n_reactive} ({country_summ()$pct_reactive} %) reactive campaigns")
    })
    
    output$n_doses <- renderText({
      glue::glue("{fmt_n_dose(country_summ()$n_d1 + country_summ()$n_d2 )}")
    })
    
    output$doses_info <- renderUI({
      tagList(
        p(glue::glue("{fmt_n_dose(country_summ()$n_d1)} first doses")),
        p(glue::glue("{fmt_n_dose(country_summ()$n_d2)} second doses"))
      )
    })
    
    output$n_areas <- renderUI({
      
    })
    
    output$areas_info <- renderUI({

      tagList(
        p(glue::glue("{unique_admin()$unique_adm1} admin 1 levels")),
        p(glue::glue("{unique_admin()$unique_adm2} admin 2 levels")),
        p(glue::glue("{unique_admin()$unique_adm3} admin 3 levels"))
      )
      
    })
    
    output$latest_campaign <- renderUI({
      tagList(
        p(glue::glue("from {latest_camp()$d1_date_start} to {latest_camp()$d2_date_end}"))
      )
    })
    
    output$latest_campaign_info <- renderUI({
      
      tagList( 
        p(glue::glue("{latest_camp()$campaign_type} campaign")),
        p(glue::glue("targeting {latest_camp()$target_type }", " ({fmt_n_dose(latest_camp()$target_pop)} people)")),
        p(glue::glue("{latest_camp()$cov_d1}% first dose coverage")), 
        p(glue::glue("{latest_camp()$cov_d2}% second dose coverage")),
        p(glue::glue("{latest_camp()$drop_out}% dropout rate"))
      )

    })
    
    # GRAPHICS/TABLES ==========================
    output$timevis <- timevis::renderTimevis({
      df_tv <- df_country_profile %>% 
        filter(
          country_code == input$country,
          between(t_d1_date_round_start, as.Date(input$time_period[1]), as.Date(input$time_period[2]))
        ) %>% 
        select(group = t_r_id, contains("date_round")) %>% 
        tibble::rowid_to_column() %>% 
        pivot_longer(
          contains("date_round"),
          names_to = "dose",
          values_to = "date"
        ) %>% 
        separate(dose, c("dose", "time"), sep = "_date_round_") %>% 
        mutate(content = str_to_upper(str_remove(dose, "t_"))) %>% 
        pivot_wider(names_from = "time", values_from = "date") %>% 
        group_by(group, content) %>% 
        summarise(
          start = min(start, na.rm = FALSE),
          end = min(end, na.rm = FALSE),
          .groups = "drop"
        ) %>% 
        drop_na(start) %>% 
        mutate(
          title = str_replace(content, "D", "Dose "),
          style = if_else(content == "D1", "background: steelblue;", "background: orange;")
        )

      df_groups <- distinct(df_tv, id = group) %>%
        mutate(content = glue::glue("<b>{id}</b>")) %>%
        arrange(id)
      
      date_range <- range(c(df_tv$start, df_tv$end), na.rm = TRUE)
      
      timevis(
        data = df_tv,
        group = df_groups,
        options = list(
          start = date_range[1] - 60,
          end = date_range[2] + 60
        )
      )
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
    
    summarise(across(where(is.numeric), ~ sum(.x)), .groups = "drop") %>% 
    
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
      cov_d1 = round(digits = 2, n_d1/target_pop),
      n_d2 = sum(d2_dose_adm), 
      cov_d2 = round(digits = 2, n_d2/target_pop), 
      drop_out = round(digits = 2, (n_d2 - n_d1) / n_d1 * 100)
    ) 
  
  
  return(camp_sum)
}

# function that summarize all data for each country using the summary for campaigns
get_country_summ <- function(camp_sum_df) {
  
  coun_sum <- camp_sum_df %>% 
    
    group_by(country_name) %>% 
    
    summarise(
      n_campaigns = n(),
      n_reactive = sum(campaign_type == "Reactive"), 
      pct_reactive = round(digits = 0, n_reactive/n_campaigns * 100),
      n_preventive = sum(campaign_type == "Preventive"),
      pct_preventive = round(digits = 0, n_preventive/n_campaigns * 100),
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
    summarise(
      unique_adm1 = n_distinct(adm1_name),
      unique_adm2 = n_distinct(adm2_name),
      unique_adm3 = n_distinct(adm3_name)
    )
}