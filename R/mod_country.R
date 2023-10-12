
mod_country_profile_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      shinyWidgets::pickerInput(
        inputId = ns("country"),
        label = "Country",
        choices = c("Cameroon" = "CMR") #na.omit(""),
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
          class = "d-flex justify-content-between align-items-center",
          "Bar chart",
          
          div(class = "pe-1", 
              shinyWidgets::radioGroupButtons(
                ns("bar_unit"),
                label = NULL,
                choices = bar_var,
                selected = "n",
                size = "sm",
                status = "outline-success"
              ))
        ),
        
        bslib::card_body(highcharter::highchartOutput(ns("chart")))
      ),
      
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          "Map",
          
          bslib::popover(
            tags$span(bs_icon("gear"), "options"),
            title = "Map options",
            placement = "left",
            
            shinyWidgets::pickerInput(
              ns("campaign"),
              label = "Select a campaign",
              choices = NULL,
              options = picker_opts(actions = FALSE, search = FALSE),
              width = 100,
              multiple = TRUE
            ), 
            
            shinyWidgets::radioGroupButtons(
              ns("admin_level"),
              label = "Admin level",
              choices = c("1" = "adm1", "2" = "adm2", "3" = "adm3"),
              size = "sm",
              status = "outline-success"
            ),
            
            shinyWidgets::radioGroupButtons(
              ns("choro_var"),
              label = "Colour variable",
              choices = c("Coverage", "Last round"),
              size = "sm",
              status = "outline-success"
            ),
            
            shinyWidgets::radioGroupButtons(
              ns("dose_var"),
              label = "Circles variable",
              choices = c("Dose 1" = "d1", "Dose 2" = "d2"),
              size = "sm",
              status = "outline-success"
            ),
            
            
            shinyWidgets::pickerInput(
              ns("map_agg"),
              label = "Aggregating rule",
              choices = c("Latest campaign" = "latest_date", "Greatest target population" = "largest_pop"),
              options = picker_opts(actions = FALSE, search = FALSE),
              width = 100,
              selected = "latest_date",
              multiple = FALSE
            )
          )
        ),
        
        bslib::card_body(padding = 0, leaflet::leafletOutput(ns("map")))
        
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
    
    # PREPARE DATA =============================
    
    #prepare the data
    prep_dat <- prep_data(df_country_profile)
    
    #filter the data for the selected country and date range
    country_df <- reactive({ prep_dat %>% 
        
        filter(country_code == input$country,
               
               between(date_start_d1, as.Date(input$time_period[1]), as.Date(input$time_period[2]) )
        ) })
    
    #summarise the campaigns (using request id) for the country df
    request_summ <-  reactive ({ get_request_summ(country_df()) })
    
    #get the latest campaign
    latest_camp <- reactive( { 
      
      request_summ() %>% 
        
        filter(date_end_d2 == max(date_end_d2) ) })
    
    #summarise all rounds using the request summary
    rounds_summ <- reactive({ get_rounds_summ(request_summ())})
    
    #get unique admin targeted for the country_df
    unique_admin <- reactive({ get_unique_admin(country_df()) } )
    
    # OBSERVERS ================================
    
    observeEvent(country_df(), {
      shinyWidgets::updatePickerInput(
        session,
        "campaign",
        choices = request_summ()$request_id
      ) })
    
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
      
      tagList( p( glue::glue(" {fmt_n_dose(sum(request_summ()$total_doses))}") ) )
      
    })
    
    output$doses_info <- renderUI({
      
      tagList( p(glue::glue("{fmt_n_dose( sum(request_summ()$n_d1) )} first doses")) ) 
      
    })
    
    output$n_areas <- renderUI({  
      
      tagList(p(glue::glue("{unique_admin()$unique_adm3} admin 3")))
      
    })
    
    output$areas_info <- renderUI({
      
      tagList( 
        p(glue::glue("{unique_admin()$unique_adm2} admin 2")), 
        p(glue::glue("{unique_admin()$unique_adm1} admin 1"))
        
      ) 
    })
    
    # TIMEVIS/BARCHART ==========================
    
    output$timevis <- timevis::renderTimevis({
      
    })
    
    output$chart <- highcharter::renderHighchart({
      
      
      y_lab <- names(bar_var[bar_var == isolate(input$bar_unit)])
      
      rounds_summ() %>% 
        
        mutate( 
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
        
        hc_plotOptions(column = list( stacking = NULL) ) %>% 
        
        hc_yAxis(title = list(text = y_lab), crosshair = FALSE) %>%
        
        hc_xAxis(title = list(text = "Request id"), crosshair = FALSE) %>%
        
        hc_tooltip(formatter = JS(
          "
    function(){ 
    outHTML =  '<b>' + this.point.request_id + '</b> - <i>' + this.point.dose +'<br>' + this.point.n_rounds + ' ' + 
    this.point.campaign_type + '</i> campaign' +  '<br><b>Target:</b> ' + this.point.target_type + 
    '(' + this.point.target_pop + ')<br>' + '<b>Doses administered:</b> ' + this.point.n_label + '<br> <b>Coverage:</b> ' + this.point.cov_label
    
    return(outHTML)
    }
    " ) ) %>% 
        
        my_hc_export()
      
    })
    
    # MAPS and TABLE ============================================
    
    #prepare the data for map
    map_df <- reactive({ 
      
      if(length(input$campaign) ) {
        
        map_df <- get_map_data(filter(country_df(), 
                                      request_id %in% input$campaign), 
                               
                               filter_var = input$map_agg, 
                               admin_level = input$admin_level)
      } else {
        
        map_df <- get_map_data(country_df(), 
                               
                               filter_var = input$map_agg, 
                               
                               admin_level = input$admin_level) }
      return(map_df)
      
    } )
    
    
    #geo reference the map_df using geodata and the input$admin_level
    geo_select <- reactive({
      geo_data[[input$admin_level]]
    })
    
    rv <- reactiveValues()
    
    observe({
      geo_join <- geo_select()$join_by
      geo_col <- unname(geo_join)
      geo_col_sym <- rlang::sym(geo_col)
      geo_name_col <- geo_select()$name_var
      geo_name_col_sym <- rlang::sym(geo_name_col)
      geo_level_name <- geo_select()$level_name
      
      # filter sf polygons to only those found in dataset
      sf <- geo_select()$sf
      
      sf <- inner_join(sf, map_df(), by = geo_join)
      
      #sf <- suppressMessages(sf::st_filter(sf, affected))
      
      # save as reactive values
      rv$geo_join <- geo_join
      rv$geo_col <- geo_col
      rv$geo_col_sym <- geo_col_sym
      rv$geo_name_col <- geo_name_col
      rv$geo_name_col_sym <- geo_name_col_sym
      rv$geo_level_name <- geo_level_name
      rv$sf <- sf
    })
    
    #MAP
    output$map <- leaflet::renderLeaflet({
      
      # map of district 
      bbox <- sf::st_bbox( isolate(filter(sf_world, country == "Cameroon")) )  
      
      leaflet::leaflet() %>%
        leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) %>%
        leaflet::addMapPane(name = "choropleth", zIndex = 300) %>%
        leaflet::addMapPane(name = "circles", zIndex = 420) %>%
        leaflet::addMapPane(name = "geo_highlight", zIndex = 430) %>%
        leaflet::addMapPane(name = "place_labels", zIndex = 310) %>%
        leaflet::addProviderTiles("CartoDB.PositronNoLabels") %>%
        leaflet::addProviderTiles("CartoDB.PositronOnlyLabels", options = leaflet::leafletOptions(pane = "place_labels"), group = "Labels") %>%
        #leaflet::addProviderTiles("OpenStreetMap", group = "OSM") %>%
        #leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM HOT") %>%
        leaflet::addScaleBar(position = "bottomleft") %>%
        leaflet.extras::addFullscreenControl(position = "topleft") %>%
        leaflet.extras::addResetMapButton() %>% 
        leaflet::addLayersControl(
          # baseGroups = c("Coverage", "Last round"),
          overlayGroups = c("Choropleth", "Circles", "Labels"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
    })
    
    #Observe the map 
    observe({
      
      # req(map_df(), rv$sf, input$dose_var)
      
      boundaries <- rv$sf
      
      cov_var <- paste0("cov_", input$dose_var)
      last_round_var <- paste0(input$dose_var, "_datecat")
      
      
      # Call the color function 
      cov_pal <- colorNumeric("YlOrRd",  domain = boundaries[[cov_var]])
      last_round_pal <- colorFactor("Dark2", domain = last_round_cat )
      
      leaflet::leafletProxy("map", session) %>%
        
        leaflet::clearGroup("Choropleth") %>% 
        
        leaflet::clearControls()
      
      # req(nrow(boundaries) > 0)
      
      if (input$choro_var == "Coverage") {
        leaflet::leafletProxy("map", session) %>%
          leaflet::addPolygons(
            data = boundaries,
            fillColor = ~ cov_pal(boundaries[[cov_var]]),
            fillOpacity = .7,
            color = "black",
            stroke = TRUE,
            weight = 1,
            label = boundaries[[cov_var]], # boundaries[[rv$geo_name_col]],
            group = "Choropleth",
            highlightOptions = leaflet::highlightOptions(bringToFront = TRUE, weight = 3),
            options = leaflet::pathOptions(pane = "choropleth")
          ) %>%
          leaflegend::addLegendNumeric(
            title = "Dose coverage",
            pal = cov_pal,
            values = boundaries[[cov_var]],
            position = 'bottomright',
            orientation = 'vertical',
            shape = 'stadium',
            decreasing = TRUE,
            height = 100,
            width = 20,
            fillOpacity = .7,
            group = "Choropleth"
          ) 
      } else {
        leaflet::leafletProxy("map", session) %>%
          leaflet::addPolygons(
            data = boundaries,
            fillColor = ~ last_round_pal(boundaries[[last_round_var]]),
            fillOpacity = .7,
            color = "black",
            stroke = TRUE,
            weight = 1,
            label = boundaries[[last_round_var]], # boundaries[[rv$geo_name_col]],
            group = "Choropleth",
            highlightOptions = leaflet::highlightOptions(bringToFront = TRUE, weight = 3),
            options = leaflet::pathOptions(pane = "choropleth")
          ) %>% 
          leaflegend::addLegendFactor(
            title = "Time since last round",
            pal = last_round_pal,
            values = boundaries[[last_round_var]],
            position = 'bottomright',
            width = 25,
            height = 25,
            shape = "rect",
            group = "Choropleth",
            opacity = .7
          )
      }
      
    })
    
    observe({
      boundaries <- rv$sf
      bbox <- sf::st_bbox(boundaries)
      n_dose <- paste0(input$dose_var, "_dose_adm")
      #circle width
      pie_width <- 60 * sqrt(boundaries[[n_dose]]) / sqrt(max(boundaries[[n_dose]]))
      
      leaflet::leafletProxy("map", session) %>%
<<<<<<< HEAD
        
        leaflet::addPolygons(
          data = boundaries,
          fillColor = ~ cov_pal(boundaries[[cov_var]]),
          fillOpacity = .7,
          color = "black",
          stroke = TRUE,
          weight = 1,
          label = boundaries[[rv$geo_name_col]],
          group = "Coverage",
          highlightOptions = leaflet::highlightOptions(bringToFront = TRUE, weight = 3),
          options = leaflet::pathOptions(pane = "choropleth")
          
        ) %>%
        
        leaflet::addLegend(
          title = "Dose coverage (%)",
          data = boundaries,
          pal = cov_pal,
          values = ~ 0:100,
          opacity = .7,
          position = "bottomright",
          group = "Coverage",
          layerId = "cov_leg",
          na.label = "No data"
        ) %>% 
        
        leaflet::addPolygons(
          data = boundaries,
          fillColor = ~ last_round_pal(boundaries[[last_round_var]]),
          fillOpacity = .7,
          color = "black",
          stroke = TRUE,
          weight = 1,
          label = boundaries[[rv$geo_name_col]],
          group = "Last round",
          highlightOptions = leaflet::highlightOptions(bringToFront = TRUE, weight = 3),
          options = leaflet::pathOptions(pane = "choropleth")
          
        ) %>% 
        
        leaflet::addLegend(
          title = "Time since last round",
          data = boundaries,
          pal = last_round_pal,
          values = ~ last_round_cat,
          opacity = .7,
          position = "bottomright",
          group = "Last round",
          layerId = "last_round_leg",
          na.label = "No data"
        ) %>%  
        
=======
        leaflet.minicharts::clearMinicharts() %>% 
>>>>>>> dad30d741e0a73fdd84a7d7467459e5d63eaec72
        leaflet.minicharts::addMinicharts(
          lng = boundaries$lon,
          lat = boundaries$lat,
          chartdata = boundaries[[n_dose]], 
<<<<<<< HEAD
          opacity = .9,
          #fillColor = pal10[1],
          #colorPalette = pal10,
          legend = TRUE,x
=======
          layerId = boundaries[[isolate(rv$geo_name_col)]],
          opacity = .7,
          legend = TRUE,
>>>>>>> dad30d741e0a73fdd84a7d7467459e5d63eaec72
          showLabels = TRUE,
          type = "pie",
          width = pie_width
        ) %>% 
        leaflet::flyToBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    })
    
    observeEvent(input$map_groups, {
      boundaries <- isolate(rv$sf)
      if (!"Circles" %in% input$map_groups) {
        leaflet::leafletProxy("map", session) %>%
          leaflet.minicharts::updateMinicharts(
            layerId = boundaries[[isolate(rv$geo_name_col)]],
            chartdata = 1,
            showLabels = FALSE,
            height = 0,
            width = 0
          )
      } else {
        n_dose <- paste0(input$dose_var, "_dose_adm")
        pie_width <- 60 * sqrt(boundaries[[n_dose]]) / sqrt(max(boundaries[[n_dose]]))
        leaflet::leafletProxy("map", session) %>%
          leaflet.minicharts::updateMinicharts(
            layerId = boundaries[[isolate(rv$geo_name_col)]],
            chartdata = boundaries[[n_dose]],
            opacity = .7,
            showLabels = TRUE,
            type = "pie",
            width = pie_width
          )
      }
    })
    
    
    #TABLE
    output$tbl <- reactable::renderReactable({
      
      if(length(input$campaign) ) {
        
        summ_tab_data( filter(request_summ(), 
                              request_id %in% input$campaign ) ) %>% 
          
          reactable()
        
      } else {
        
        summ_tab_data(request_summ()) %>% reactable() 
        
      }
      
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
    
    summarise(across(is.numeric, ~ sum(.x)), 
              .groups = "drop" ) %>% 
    
    mutate(
      d1_cov_adm = d1_dose_adm/target_pop,
      d2_cov_adm = d2_dose_adm/target_pop)
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


# Function to get unique number of admin area targetted per country 
get_unique_admin <- function(country_df) {
  
  country_df %>% 
    
    summarise(unique_adm1 = n_distinct(adm1_name), 
              unique_adm2 = n_distinct(adm2_name), 
              unique_adm3 = n_distinct(adm3_name), 
              .groups = "drop" ) }

#function to create summary table from the requests summary data
summ_tab_data <- function(df) {
  
  df %>% 
    
    mutate( across( contains("date_"), ~ format(.x, '%d/%m/%Y')), 
            date_range_d1 = glue::glue("{date_start_d1} - {date_end_d1}"),
            date_range_d2 = glue::glue("{date_start_d2} - {date_end_d2}"), 
            
            across(c(n_d1, n_d2, target_pop, total_doses), ~ fmt_n_dose(.x)), 
            cov_d1 = glue::glue("{cov_d1} %"), 
            cov_d2 = glue::glue("{cov_d2} %")
    ) %>% 
    
    select( 
      "Request id" = request_id,
      "Campaign type" = campaign_type, 
      "Number of rounds" = n_rounds, 
      "Number of targeted areas" = n_target_area, 
      "Smallest area recorded"  = smallest_target, 
      "Target population" = target_type, 
      "Size of target population" = target_pop, 
      "Dose 1 round dates" = date_range_d1, 
      "Number of doses 1 administered" = n_d1, 
      "Coverage of doses 1" = cov_d1, 
      "Dose 2 round dates" = date_range_d2, 
      "Number of doses 2 administered" = n_d2, 
      "Coverage of doses 2" = cov_d2, 
      "Total doses" = total_doses,
      
      -c(country_name, date_start_d1, date_end_d1, date_start_d2, date_end_d2, contains("n_adm"))
    
    ) %>% 
    
    t() %>%
    
    janitor::row_to_names(1) 
  
}

#get the map data filtered and grouped

get_map_data <- function(country_df, filter_var, admin_level){
  
  n_rounds <- unique(country_df$n_rounds)
  
  admin_sym <- sym(paste0(admin_level, "_name"))
  
  df <- country_df %>% 
    
    group_by(request_id, !!admin_sym ) 
  
  if(filter_var == "largest_pop") {
    
    df <- df %>% filter( target_pop == max( target_pop ) ) 
    
  } else if (filter_var == "latest_date" & n_rounds == "one dose") { 
    
    df <- df %>% filter( date_end_d1 == max(date_end_d1) )
    
  } else {
    
    df <- df %>% filter( date_end_d2 == max(date_end_d2) )
    
  }   
  
  df %>% 
    
    summarise(n_targets = n(), 
              
              date_start_d1 = min(date_start_d1),
              date_end_d1 = max(date_end_d1),
              date_start_d2 =min(date_start_d2) , 
              date_end_d2 = max(date_end_d2),
              
              across(c(target_pop,
                       d1_dose_adm, 
                       d2_dose_adm), ~ sum(.x)), 
              
              across(c(target_type, ), 
                     ~ paste0(unique(.x), collapse = ", ")), 
              
              .groups = "drop") %>%
    
    ungroup() %>% 
    
    mutate(cov_d1 = round(digits = 1, d1_dose_adm /target_pop * 100 ), 
           cov_d2 = round(digits = 1, d2_dose_adm /target_pop * 100 ), 
           
           d1_datecat = case_when(
             between(date_end_d1, Sys.Date() - 180, Sys.Date() ) ~ "0-6 months", 
             between(date_end_d1, Sys.Date() - 360, Sys.Date() - 180 ) ~ "6m-1 year", 
             between(date_end_d1,Sys.Date() - 1080, Sys.Date() - 360) ~ "1-3 year", 
             date_end_d1 <= Sys.Date() - 1080  ~ "more than 3 years") , 
           
           d2_datecat = case_when(
             between(date_end_d2, Sys.Date() - 180, Sys.Date() ) ~ "0-6 months", 
             between(date_end_d2, Sys.Date() - 360, Sys.Date() - 180 ) ~ "6m-1 year", 
             between(date_end_d2,Sys.Date() - 1080, Sys.Date() - 360) ~ "1-3 year", 
             date_end_d2 <= Sys.Date() - 1080  ~ "more than 3 years") 
    )
}

#define variables
bar_var <- c("Doses counts" = "n", "Coverage" = "cov")
last_round_cat <- c("0-6 months","6m-1 year", "1-3 year", "more than 3 years")
