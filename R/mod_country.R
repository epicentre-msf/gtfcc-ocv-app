
mod_country_profile_ui <- function(id) {
  ns <- NS(id)
  start_date <- as.Date("2021-07-03")
  end_date <- as.Date("2023-02-01")
  # test_date <- as.Date("2021-12-01")
  div(
    class = "container",
    # bslib::layout_sidebar(
    #   fillable = FALSE,
    #   sidebar = bslib::sidebar(
    #     shinyWidgets::pickerInput(
    #       inputId = ns("country"),
    #       label = "Country",
    #       choices = c("Cameroon" = "CMR"),
    #       options = picker_opts()
    #     ),
    #     shiny::sliderInput(
    #       inputId = ns("time_period"),
    #       label = "Time period",
    #       min = as.Date("2021-07-03"),
    #       max = as.Date("2023-02-01"),
    #       value = c(as.Date("2021-07-03"), as.Date("2023-02-01")),
    #       width = "100%",
    #       timeFormat = "%d/%m/%y"
    #     )
    #   ),
    bslib::layout_columns(
      col_widths = c(-2, 4, 4, -2),
      shinyWidgets::pickerInput(
        inputId = ns("country"),
        label = tags$h5("Select country"),
        choices = c("Cameroon" = "CMR"),
        options = picker_opts(style = "btn-lg btn-outline-success")
      ),
      shiny::sliderInput(
        inputId = ns("time_period"),
        label = tags$h5("Time period"),
        min = start_date,
        max = end_date,
        value = c(start_date, end_date),
        width = "100%",
        timeFormat = "%d/%m/%y"
      )
    ),
    bslib::layout_column_wrap(
      width = 1 / 3,
      fill = FALSE,
      bslib::value_box(
        title = "Campaigns",
        value = textOutput(ns("n_campaigns")),
        htmlOutput(ns("campaigns_info")),
        theme_color = "success",
        # showcase = health_icon("symbols/cholera", type = "outline", height = 80),
        showcase = bs_icon("droplet")
      ),
      bslib::value_box(
        title = "Doses",
        value = textOutput(ns("n_doses")),
        textOutput(ns("doses_info")),
        theme_color = "success",
        # showcase = health_icon("medications/pills_2", type = "outline", height = 80)
        showcase = bs_icon("prescription2")
      ),
      bslib::value_box(
        title = "Targeted areas",
        value = textOutput(ns("n_areas")),
        htmlOutput(ns("areas_info")),
        theme_color = "success",
        # showcase = health_icon("symbols/geo_location", type = "outline", height = 80)
        showcase = bs_icon("geo")
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6, 6, 6),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex justify-content-start align-items-center",
          "Campaign timeline"
        ),
        bslib::card_body(padding = 0, timevis::timevisOutput(ns("timevis")))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          "Bar chart",
          div(
            class = "pe-1",
            shinyWidgets::radioGroupButtons(
              ns("bar_unit"),
              label = NULL,
              choices = bar_var,
              selected = "n",
              size = "sm",
              status = "outline-success"
            )
          )
        ),
        bslib::card_body(padding = 0, highcharter::highchartOutput(ns("chart")))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex align-items-center",
          tags$span(class = "me-auto pe-1 ", "Map"),
          div(class = "pe-2", shinyWidgets::pickerInput(
            ns("campaign"),
            label = NULL,
            choices = NULL,
            options = picker_opts(actions = FALSE, search = FALSE, none_text = "All campaigns"),
            width = 100,
            multiple = TRUE
          )),
          bslib::popover(
            tags$span(class = "pe-1", bs_icon("gear"), "options"),
            title = "Map options",
            placement = "left",
            shinyWidgets::radioGroupButtons(
              ns("admin_level"),
              label = "Admin level",
              choices = c("ADM1" = "adm1", "ADM2" = "adm2", "ADM3" = "adm3"),
              size = "sm",
              status = "outline-success"
            ),
            shinyWidgets::radioGroupButtons(
              ns("dose_var"),
              label = "Dose",
              choices = c("Dose 1" = "d1", "Dose 2" = "d2"),
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
            shinyWidgets::pickerInput(
              ns("map_agg"),
              label = "Aggregating rule",
              choices = c("Latest campaign" = "latest_date", "Greatest target population" = "largest_pop"),
              options = picker_opts(actions = FALSE, search = FALSE),
              width = "100%",
              selected = "latest_date",
              multiple = FALSE
            )
          )
        ),
        bslib::card_body(padding = 0, leaflet::leafletOutput(ns("map")))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex justify-content-start align-items-center",
          "Summary table"
        ),
        bslib::card_body(padding = 0, reactable::reactableOutput(ns("tbl")))
      )
    )
  )
}

mod_country_profile_server <- function(id, df_country_profile) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # PREPARE DATA =============================
    
    # prepare the data
    prep_dat <- prep_data(df_country_profile)
    
    # filter the data for the selected country and date range
    country_df <- reactive({
      prep_dat %>%
        filter(
          country_code == input$country,
          between(date_start_d1, as.Date(input$time_period[1]), as.Date(input$time_period[2]))
        )
    })
    
    # summarise the campaigns (using request id) for the country df
    request_summ <- reactive({
      req(nrow(country_df()) > 0)
      get_request_summ(country_df())
    })
    
    # get the latest campaign
    latest_camp <- reactive({
      req(nrow(country_df()) > 0)
      request_summ() %>%
        filter(date_end_d2 == max(date_end_d2))
    })
    
    # summarise all rounds using the request summary
    rounds_summ <- reactive({
      req(request_summ())
      get_rounds_summ(request_summ())
    })
    
    # get unique admin targeted for the country_df
    unique_admin <- reactive({
      req(nrow(country_df()) > 0)
      get_unique_admin(country_df())
    })
    
    # OBSERVERS ================================
    
    observeEvent(country_df(), {
      shinyWidgets::updatePickerInput(
        session,
        "campaign",
        choices = request_summ()$request_id
      )
    })
    
    # VALUE BOXES ==============================
    
    output$n_campaigns <- renderText({
      get_rounds_summ(request_summ())
      glue::glue("{nrow(request_summ())}")
    })
    
    output$campaigns_info <- renderUI({
      get_rounds_summ(request_summ())
      HTML(glue::glue(
        "{fmt_count(request_summ(), campaign_type == 'Reactive')} reactive campaigns</br>
         {fmt_count(request_summ(), n_rounds == 'single dose')} single dose campaigns"
      ))
    })
    
    output$n_doses <- renderText({
      get_rounds_summ(request_summ())
      glue::glue("{fmt_n_dose(sum(request_summ()$total_doses))}")
    })
    
    output$doses_info <- renderText({
      get_rounds_summ(request_summ())
      glue::glue("{fmt_n_dose( sum(request_summ()$n_d1) )} first doses")
    })
    
    output$n_areas <- renderText({
      get_rounds_summ(request_summ())
      glue::glue("{unique_admin()$unique_adm3} admin 3")
    })
    
    output$areas_info <- renderUI({
      get_rounds_summ(request_summ())
      HTML(glue::glue(
        "{unique_admin()$unique_adm2} admin 2</br>
         {unique_admin()$unique_adm1} admin 1"
      ))
    })
    
    # TIMEVIS/BARCHART ==========================
    
    output$timevis <- timevis::renderTimevis({
      validate(need(nrow(country_df()) > 0, "No data to display"))
      df_tv <- country_df() %>%
        select(group = request_id, contains("date_")) %>%
        tibble::rowid_to_column() %>%
        pivot_longer(
          contains("date_"),
          names_to = "dose",
          values_to = "date"
        ) %>%
        mutate(dose = str_remove(dose, "date_")) %>%
        separate(dose, c("time", "dose"), sep = "_") %>%
        mutate(content = str_to_upper(dose)) %>%
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
        ),
        height = 380
      )
    })
    
    output$chart <- highcharter::renderHighchart({
      validate(need(isTruthy(nrow(rounds_summ()) > 0), "No data to display"))
      
      y_lab <- names(bar_var[bar_var == isolate(input$bar_unit)])
      
      rounds_summ() %>%
        mutate(
          campaign_type = str_to_lower(campaign_type),
          target_pop = fmt_n_dose(target_pop),
          total_doses = fmt_n_dose(total_doses),
          cov_label = scales::percent(cov, scale = 1),
          n_label = fmt_n_dose(n),
          dose = case_match(
            dose,
            "d1" ~ "Dose 1",
            "d2" ~ "Dose 2"
          )
        ) %>%
        hchart(
          .,
          "column",
          hcaes(
            x = request_id,
            y = !!sym(input$bar_unit),
            group = dose
          )
        ) %>%
        hc_plotOptions(column = list(stacking = NULL)) %>%
        hc_yAxis(title = list(text = y_lab), crosshair = FALSE) %>%
        hc_xAxis(title = list(text = "Request id"), crosshair = FALSE) %>%
        hc_tooltip(formatter = JS(
          "function(){
            outHTML =  '<b>' + this.point.request_id + '</b> - <i>' + this.point.dose +'<br>' + this.point.n_rounds + ' ' +
            this.point.campaign_type + '</i> campaign' +  '<br><b>Target:</b> ' + this.point.target_type +
            '(' + this.point.target_pop + ')<br>' + '<b>Doses administered:</b> ' + this.point.n_label + '<br> <b>Coverage:</b> ' + this.point.cov_label
        
            return(outHTML)
            }
            "
        )) %>%
        my_hc_export()
    })
    
    # MAPS and TABLE ============================================
    
    # prepare the data for map
    map_df <- reactive({
      if (length(input$campaign)) {
        map_df <- get_map_data(
          filter(
            country_df(),
            request_id %in% input$campaign
          ),
          filter_var = input$map_agg,
          admin_level = input$admin_level
        )
      } else {
        map_df <- get_map_data(
          country_df(),
          filter_var = input$map_agg,
          admin_level = input$admin_level
        )
      }
      return(map_df)
    })
    
    
    # geo reference the map_df using geodata and the input$admin_level
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
      
      # save as reactive values
      rv$geo_join <- geo_join
      rv$geo_col <- geo_col
      rv$geo_col_sym <- geo_col_sym
      rv$geo_name_col <- geo_name_col
      rv$geo_name_col_sym <- geo_name_col_sym
      rv$geo_level_name <- geo_level_name
      rv$sf <- sf
    })
    
    # MAP
    output$map <- leaflet::renderLeaflet({
      
      bbox <- sf::st_bbox(isolate(filter(sf_world, country == "Cameroon")))
      
      leaflet::leaflet() %>%
        leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) %>%
        leaflet::addMapPane(name = "choropleth", zIndex = 300) %>%
        leaflet::addMapPane(name = "circles", zIndex = 420) %>%
        leaflet::addMapPane(name = "geo_highlight", zIndex = 430) %>%
        leaflet::addMapPane(name = "place_labels", zIndex = 310) %>%
        leaflet::addProviderTiles("CartoDB.PositronNoLabels") %>%
        leaflet::addProviderTiles(
          "CartoDB.PositronOnlyLabels",
          options = leaflet::leafletOptions(pane = "place_labels"),
          group = "Labels"
        ) %>%
        leaflet::addScaleBar(position = "bottomleft") %>%
        leaflet::addLayersControl(
          position = "topleft",
          overlayGroups = c("Choropleth", "Doses", "Labels"),
          options = layersControlOptions(collapsed = FALSE)
        )
      # leaflet.extras::addResetMapButton() %>%
      # leaflet::addProviderTiles("OpenStreetMap", group = "OSM") %>%
      # leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM HOT") %>%
      # leaflet.extras::addFullscreenControl(position = "topleft") %>%
    })
    
    # Observe the map
    observe({
      
      leaflet::leafletProxy("map", session) %>%
        leaflet::clearGroup("Choropleth") %>%
        leaflet::clearControls()
      
      boundaries <- rv$sf
      req(nrow(isolate(boundaries)) > 0)
      
      cov_var <- paste0("cov_", input$dose_var)
      last_round_var <- paste0(input$dose_var, "_datecat")
      
      tt <- get_tooltip(
        df = boundaries,
        admin = rv$geo_name_col_sym,
        dose_type = input$dose_var
      )
      
      # Call the color function
      cov_pal <- colorNumeric("YlOrRd", domain = boundaries[[cov_var]])
      last_round_pal <- colorFactor("Dark2", domain = last_round_cat)
      
      if (input$choro_var == "Coverage") {
        leaflet::leafletProxy("map", session) %>%
          leaflet::addPolygons(
            data = boundaries,
            fillColor = ~ cov_pal(boundaries[[cov_var]]),
            fillOpacity = .7,
            color = "black",
            stroke = TRUE,
            weight = 1,
            label = tt, 
            group = "Choropleth",
            highlightOptions = leaflet::highlightOptions(bringToFront = TRUE, weight = 3),
            options = leaflet::pathOptions(pane = "choropleth")
          ) %>%
          leaflet::addLegend(
            title = "Dose coverage",
            pal = cov_pal,
            values = boundaries[[cov_var]],
            position = "bottomright",
            opacity = .7,
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
            label = tt,
            group = "Choropleth",
            highlightOptions = leaflet::highlightOptions(bringToFront = TRUE, weight = 3),
            options = leaflet::pathOptions(pane = "choropleth")
          ) %>%
          leaflet::addLegend(
            title = "Time since last round",
            pal = last_round_pal,
            values = boundaries[[last_round_var]],
            position = "bottomright",
            opacity = .7,
            group = "Choropleth"
          )
      }
      
      bbox <- sf::st_bbox(boundaries)
      leaflet::leafletProxy("map", session) %>%
        leaflet::flyToBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    })
    
    minicharts_init <- reactiveVal(TRUE)
    
    observe({
      
      if (isTruthy("Doses" %in% isolate(input$map_groups)) || minicharts_init())  {
        leaflet::leafletProxy("map", session) %>%
          leaflet.minicharts::clearMinicharts()
        
        # req(nrow(isolate(country_df())) > 0)
        boundaries <- rv$sf
        req(nrow(isolate(boundaries)) > 0)
        n_dose <- paste0(input$dose_var, "_dose_adm")
        # circle width
        pie_width <- 60 * sqrt(boundaries[[n_dose]]) / sqrt(max(boundaries[[n_dose]]))
        
        leaflet::leafletProxy("map", session) %>%
          leaflet.minicharts::addMinicharts(
            lng = boundaries$lon,
            lat = boundaries$lat,
            chartdata = boundaries[[n_dose]],
            legend = TRUE,
            layerId = boundaries[[isolate(rv$geo_name_col)]],
            opacity = .7,
            showLabels = TRUE,
            type = "pie",
            width = pie_width
          )
        
        minicharts_init(FALSE)
      } else {
        leaflet::leafletProxy("map", session) %>%
          leaflet.minicharts::clearMinicharts()
        # leaflet.minicharts::updateMinicharts(
        #   layerId = boundaries[[isolate(rv$geo_name_col)]],
        #   chartdata = 1,
        #   showLabels = FALSE,
        #   height = 0,
        #   width = 0
        # )
      }
    }) %>% bindEvent(rv$sf, input$dose_var, rv$geo_name_col)
    
    observeEvent(input$map_groups, {
      boundaries <- isolate(rv$sf)
      if (!"Doses" %in% input$map_groups) {
        leaflet::leafletProxy("map", session) %>%
          leaflet.minicharts::clearMinicharts() 
        # leaflet.minicharts::updateMinicharts(
        #   layerId = boundaries[[isolate(rv$geo_name_col)]],
        #   chartdata = 1,
        #   showLabels = FALSE,
        #   height = 0,
        #   width = 0
        # )
      } else {
        req(nrow(isolate(boundaries)) > 0)
        n_dose <- paste0(input$dose_var, "_dose_adm")
        pie_width <- 60 * sqrt(boundaries[[n_dose]]) / sqrt(max(boundaries[[n_dose]]))
        leaflet::leafletProxy("map", session) %>%
          leaflet.minicharts::addMinicharts(
            lng = boundaries$lon,
            lat = boundaries$lat,
            chartdata = boundaries[[n_dose]],
            legend = TRUE,
            layerId = boundaries[[isolate(rv$geo_name_col)]],
            opacity = .7,
            showLabels = TRUE,
            type = "pie",
            width = pie_width
          )
        # leaflet.minicharts::updateMinicharts(
        #   layerId = boundaries[[isolate(rv$geo_name_col)]],
        #   chartdata = boundaries[[n_dose]],
        #   opacity = .7,
        #   showLabels = TRUE,
        #   type = "pie",
        #   width = pie_width
        # )
      }
    })
    
    # TABLE
    output$tbl <- reactable::renderReactable({
      validate(need(isTruthy(nrow(request_summ()) > 0), "No data to display"))
      if (length(input$campaign)) {
        df_rt <- request_summ() %>% 
          filter(request_id %in% input$campaign) %>% 
          summ_tab_data()
      } else {
        df_rt <- request_summ() %>% summ_tab_data()
      }
      reactable(
        df_rt,
        highlight = TRUE,
        compact = TRUE,
        pagination = FALSE, 
        height = 400
      )
    })
  })
}


# Functions for module ===============================================

# function to prepare the data for country profile
prep_data <- function(target_area_df) {
  
  # make new id
  target_area_df %>%
    select(
      request_id = t_r_id,
      country_code,
      country_name = t_r_country,
      adm1_name = adm1_t_target_area,
      adm2_name = adm2_t_target_area,
      adm3_name = adm3_t_target_area,
      adm4_name = adm4_t_target_area,
      adm1_pcode = pcode_adm1_t_target_area,
      adm2_pcode = pcode_adm2_t_target_area,
      adm3_pcode = pcode_adm3_t_target_area,
      adm4_pcode = pcode_adm4_t_target_area,
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
    # add a variable saying if there is one or two dose for the campaign
    mutate(n_rounds = if_else(if_any(c(date_start_d1, date_start_d2), ~ is.na(.x)), "one dose", "two doses")) %>%
    # aggregate to admin3 level
    group_by(
      country_code,
      country_name,
      # camp_id,
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
      adm3_name,
      adm1_pcode,
      adm2_pcode,
      adm3_pcode
    ) %>%
    summarise(
      across(where(is.numeric), ~ sum(.x)),
      .groups = "drop"
    ) %>%
    mutate(
      d1_cov_adm = d1_dose_adm / target_pop,
      d2_cov_adm = d2_dose_adm / target_pop
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
      cov_d1 = round(digits = 1, n_d1 / target_pop * 100),
      n_d2 = sum(d2_dose_adm),
      cov_d2 = round(digits = 1, n_d2 / target_pop * 100),
      .groups = "drop"
    ) %>%
    mutate(total_doses = n_d1 + n_d2)
  
  return(req_summ)
}


# functions to get a summary of rounds for requests
get_rounds_summ <- function(request_summ_df) {
  request_summ_df %>%
    rename_with(.cols = c(date_start_d1, date_end_d1, date_start_d2, date_end_d2), ~ str_replace(.x, "_", "")) %>%
    pivot_longer(
      cols = c(n_d1, n_d2, cov_d1, cov_d2, datestart_d1, dateend_d1, datestart_d2, dateend_d2),
      names_sep = "_",
      names_to = c(".value", "dose"),
      # removes if doses values is NA
      values_drop_na = TRUE
    )
}


# Function to get unique number of admin area targetted per country
get_unique_admin <- function(country_df) {
  country_df %>%
    summarise(
      unique_adm1 = n_distinct(adm1_name),
      unique_adm2 = n_distinct(adm2_name),
      unique_adm3 = n_distinct(adm3_name),
      .groups = "drop"
    )
}

# function to create summary table from the requests summary data
summ_tab_data <- function(df) {
  df %>%
    mutate(
      across(contains("date_"), ~ format(.x, "%d/%m/%Y")),
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

# get the map data filtered and grouped

get_map_data <- function(country_df, filter_var, admin_level) {
  
  n_rounds <- unique(country_df$n_rounds)
  admin_sym <- sym(paste0(admin_level, "_pcode"))
  
  if (nrow(country_df) < 1) {
    return(dplyr::tibble(!!admin_sym := character()))
  }
  
  df <- country_df %>%
    group_by(!!admin_sym)
  
  if (filter_var == "largest_pop") {
    df <- df %>% filter(target_pop == max(target_pop))
  } else if (filter_var == "latest_date" & isTruthy(n_rounds == "one dose")) {
    df <- df %>% filter(date_end_d1 == max(date_end_d1))
  } else {
    df <- df %>% filter(date_end_d2 == max(date_end_d2))
  }
  
  df %>%
    group_by(request_id, !!admin_sym) %>% 
    summarise(
      n_targets = n(),
      date_start_d1 = min(date_start_d1),
      date_end_d1 = max(date_end_d1),
      date_start_d2 = min(date_start_d2),
      date_end_d2 = max(date_end_d2),
      across(
        c(
          target_pop,
          d1_dose_adm,
          d2_dose_adm
        ),
        ~ sum(.x)
      ),
      across(
        c(target_type),
        ~ paste0(unique(.x), collapse = ", ")
      ),
      .groups = "drop"
    ) %>%
    mutate(
      cov_d1 = round(digits = 1, d1_dose_adm / target_pop * 100),
      cov_d2 = round(digits = 1, d2_dose_adm / target_pop * 100),
      d1_datecat = case_when(
        between(date_end_d1, Sys.Date() - 180, Sys.Date()) ~ "0-6 months",
        between(date_end_d1, Sys.Date() - 360, Sys.Date() - 180) ~ "6m-1 year",
        between(date_end_d1, Sys.Date() - 1080, Sys.Date() - 360) ~ "1-3 year",
        date_end_d1 <= Sys.Date() - 1080 ~ "more than 3 years"
      ),
      d2_datecat = case_when(
        between(date_end_d2, Sys.Date() - 180, Sys.Date()) ~ "0-6 months",
        between(date_end_d2, Sys.Date() - 360, Sys.Date() - 180) ~ "6m-1 year",
        between(date_end_d2, Sys.Date() - 1080, Sys.Date() - 360) ~ "1-3 year",
        date_end_d2 <= Sys.Date() - 1080 ~ "more than 3 years"
      )
    )
}

# define variables
bar_var <- c("Doses counts" = "n", "Coverage" = "cov")
last_round_cat <- c("0-6 months", "6m-1 year", "1-3 year", "more than 3 years")

get_tooltip <- function(df, admin, dose_type) {
  n_dose <- paste0(dose_type, "_dose_adm") 
  dose_cov <- paste0("cov_", dose_type) 
  lastround_var <- paste0(dose_type, "_datecat")
  
  glue::glue(
    "<b>{df[[admin]]}</b><br>
       Last round: <b>{ df[[lastround_var]] }</b><br>
       Target population: <b>{fmt_n_dose(df$target_pop)}</b><br>
       Dose administered: <b>{ fmt_n_dose(df[[n_dose]]) }</b><br>
       Dose coverage: <b>{fmt_n_dose(df[[dose_cov]])} %</b><br>"
  ) %>% purrr::map(htmltools::HTML)
}
