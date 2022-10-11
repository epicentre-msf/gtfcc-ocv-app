
mod_request_ui <- function(id) {
  ns <- NS(id)
  delay_choices <- delay_choices(app_data$max_shipments, app_data$max_rounds)
  
  tagList(
    # pushbar inputs ============================
    pushbar_ui(ns),
    
    div(
      class = "reactive-width",
      
      fluidRow(
        column(
          width = 12, class = "header-title",
          div_inline(
            h1("GTFCC OCV Requests")
          ),
          div_inline(
            actionButton(ns("open"), "Data Filters", icon = icon("filter"), class = "btn-primary btn-sm", style = "color: #fff;")
          ),
          tags$p("")
        )
      ),
      
      # verbatimTextOutput(outputId = ns("geo_select")),
      
      fluidRow(
        shinydashboard::valueBoxOutput(ns("n_requests"), width = 3),
        shinydashboard::valueBoxOutput(ns("n_doses"), width = 3),
        shinydashboard::valueBoxOutput(ns("n_countries"), width = 3),
        shinydashboard::valueBoxOutput(ns("time_decision"), width = 3)
      ),
      
      fluidRow(
        
        column(width = 12, tagList(
          shinyWidgets::radioGroupButtons(
            ns("var"),
            label = NULL,
            choices = c("Requests", "Doses"),
            size = "sm"
          ),
          shinyWidgets::radioGroupButtons(
            ns("group"),
            label = NULL,
            choices = group_vars,
            size = "sm"
          ),
          shinyWidgets::radioGroupButtons(
            ns("dose"),
            label = NULL,
            choices = dose_vars,
            size = "sm"
          )
        ) %>% purrr::map(div_inline)),
        
        box_w_inputs(
          width = 12,
          title = tagList(shiny::icon("globe-africa")),
          input_right = capture::capture(
            selector = "#request-map-container",
            filename = glue::glue("GTFCC-Map-{Sys.Date()}.png"),
            icon("camera"),
            "Download",
            class = "btn-xs"
          ),
          footer = uiOutput(ns("map_footer")),
          tags$div(id = "request-map-container", leaflet::leafletOutput(ns("map")))
        ),
        
        box_w_inputs(
          width = 6,
          title = tagList(shiny::icon("bar-chart")),
          inputs = tagList(
            shinyWidgets::radioGroupButtons(
              ns("ts_unit"),
              label = NULL,
              choices = c("Year" = "year", "Quarter" = "quarter", "Month" = "month", "Week" = "week"),
              selected = "year",
              size = "sm"
            ),
            shinyWidgets::pickerInput(
              ns("ts_date"),
              label = NULL,
              choices = date_vars[1:2],
              selected = date_vars[1],
              options = picker_opts(actions = FALSE, search = FALSE),
              width = 150,
              multiple = FALSE
            )
          ),
          highcharter::highchartOutput(ns("ts_chart")),
          footer = uiOutput(ns("cfr_footer"))
        ),
        
        tab_box_custom(
          id = ns("delay_tabs"), width = 6, side = "right",
          title = "Delays",
          inputs = tagList(
            #helpText("from:"),
            shinyWidgets::pickerInput(
              ns("date_1"),
              label = NULL,
              choices = delay_choices,
              selected = delay_choices[1],
              options = picker_opts(actions = FALSE, search = FALSE),
              width = 100,
              multiple = FALSE
            ),
            helpText("-"),
            shinyWidgets::pickerInput(
              ns("date_2"),
              label = NULL,
              choices = delay_choices,
              selected = delay_choices[2],
              options = picker_opts(actions = FALSE, search = FALSE),
              width = 100,
              multiple = FALSE
            ),
            shinyWidgets::radioGroupButtons(
              ns("delay_stacking"),
              label = NULL,
              choices = c("Stacked bars" = "normal", "Dodged bars" = "none"),
              size = "sm"
            )
          ),
          tabPanel(shiny::icon("bar-chart"), value = "chart", highcharter::highchartOutput(ns("delay"))),
          tabPanel(shiny::icon("table"), value = "table", div(style = "min-height: 300px;", gt::gt_output(ns("delay_tbl"))))
        ),
        
        # box_w_inputs(
        #   width = 6,
        #   title = tagList("Delay distribution"),
        #   inputs = tagList(
        #     helpText("from:"),
        #     shinyWidgets::pickerInput(
        #       ns("date_1"),
        #       label = NULL,
        #       choices = delay_choices,
        #       selected = delay_choices[1],
        #       options = picker_opts(actions = FALSE, search = FALSE),
        #       width = 100,
        #       multiple = FALSE
        #     ),
        #     helpText("to:"),
        #     shinyWidgets::pickerInput(
        #       ns("date_2"),
        #       label = NULL,
        #       choices = delay_choices,
        #       selected = delay_choices[2],
        #       options = picker_opts(actions = FALSE, search = FALSE),
        #       width = 100,
        #       multiple = FALSE
        #     )
        #     # helpText("group by:"),
        #     # shinyWidgets::radioGroupButtons(
        #     #   ns("delay_group"),
        #     #   label = NULL,
        #     #   choices = c("Mechanism" = "request_mechanism", "Status" = "request_status"),
        #     #   selected = "request_mechanism",
        #     #   size = "sm"
        #     # )
        #   ),
        #   highcharter::highchartOutput(ns("delay")),
        #   footer = uiOutput(ns("delay_footer"))
        # ),
        
        box_w_inputs(
          width = 12,
          title = tagList("Request Timeline", tags$small("(select one)")),
          inputs = tagList(
            shinyWidgets::pickerInput(
              inputId = ns("demand"),
              label = NULL,
              choices = "",
              options = picker_opts(search = TRUE),
              multiple = FALSE
            )
          ),
          timevis::timevisOutput(ns("timevis"))
        )
        
      )
    )
  )
}

mod_request_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$geo_select <- renderPrint({
      input$geo
    })
    
    # ==========================================================================
    # PUSHBAR SETUP
    # ==========================================================================
    pushbar::setup_pushbar() 
    observeEvent(input$open, {
      if (input$open == 1) {
        pushbar::pushbar_open(id = ns("filters"))
      } else if (input$filters_pushbar_opened) {
        pushbar::pushbar_close()
      } else {
        pushbar::pushbar_open(id = ns("filters"))
      }
    })
    observeEvent(input$go2, { pushbar::pushbar_close() })
    observeEvent(input$reset, { shinyjs::reset("resetable_filters") })
    observeEvent(input$close, { pushbar::pushbar_close() })
    
    # ==========================================================================
    # DATA PREP
    # ==========================================================================
    
    observe({
      if (length(input$region)) {
        countries <- df_request %>% filter(who_region %in% input$region) %>% distinct(request_country) %>% pull() %>% sort()
        selected <- intersect(input$country, countries)
        shinyWidgets::updatePickerInput(session, "country", choices = countries, selected = selected)
      } else {
        countries <- unique(df_request$request_country) %>% na.omit() %>% sort()
        selected <- intersect(input$country, countries)
        shinyWidgets::updatePickerInput(session, "country", choices = countries, selected = selected)
      }
    })
    
    geo_select <- reactiveVal("World")
    observeEvent(input$geo, { geo_select(input$geo) })
    
    df_data <- reactive({
      df <- df_request %>% 
        filter(quarter >= input$q_range[1], quarter <= input$q_range[2])
      
      # if (length(input$geo)) df %<>% filter_geo(input$geo)
      if (length(input$region)) df %<>% filter(who_region %in% input$region)
      if (length(input$country)) df %<>% filter(request_country %in% input$country)
      if (length(input$status)) df %<>% filter(request_status %in% input$status)
      if (length(input$context)) df %<>% filter(context %in% input$context)
      if (length(input$mechanism)) df %<>% filter(request_mechanism %in% input$mechanism)
      if (length(input$agency)) df %<>% filter(request_agency %in% input$agency)
      # if (!is.null(country_select())) df %<>% filter(iso_a3 == country_select())
      if (length(input$vaccine)) {
        vacc_filter <- df_shipment %>% filter(vaccine %in% input$vaccine) %>% distinct(id_demand)
        df %<>% semi_join(vacc_filter, by = "id_demand")
      }
      
      return(df)
    }) %>% bindEvent(input$go1, input$go2, ignoreInit = FALSE, ignoreNULL = FALSE)
    
    # ==========================================================================
    # VALUE BOXES
    # ==========================================================================
    
    df_summary <- reactive({
      df <- df_data()
      # if (!is.null(country_select())) df %<>% filter(iso_a3 == country_select())
      df %>% 
        mutate(time_decision = date_decision - date_receipt) %>% 
        summarise(
          n_requests = n(),
          n_approved = sum(request_status == "Approved", na.rm = TRUE),
          pcnt_approved = n_approved / n_requests,
          n_dose_requested = sum(n_dose_request, na.rm = TRUE),
          n_dose_approved = sum(n_dose_approve, na.rm = TRUE),
          pcnt_dose_approved = n_dose_approved / n_dose_requested,
          n_regions = n_distinct(who_region, na.rm = TRUE),
          n_countries = n_distinct(request_country, na.rm = TRUE),
          time_decision_av = round(mean(time_decision, na.rm = TRUE), 1),
          time_decision_min = min(time_decision, na.rm = TRUE),
          time_decision_max = max(time_decision, na.rm = TRUE)
        )
    })
    
    output$n_requests <- renderValueBox({
      r <- scales::number(df_summary()$n_requests)
      a <- scales::number(df_summary()$n_approved)
      ap <- scales::percent(df_summary()$pcnt_approved)
      valueBoxSpark(
        width = 12,
        title = "Requests received",
        value = r,
        subtitle = glue::glue("{a} ({ap}) approved"),
        color = "red",
        icon = icon("list")
        # info = ""
      )
    })
    
    output$n_doses <- renderValueBox({
      r <- fmt_n_dose(df_summary()$n_dose_requested)
      a <- fmt_n_dose(df_summary()$n_dose_approved)
      ap <- scales::percent(df_summary()$pcnt_dose_approved)
      valueBoxSpark(
        width = 12,
        title = "Doses requested",
        value = r,
        subtitle = glue::glue("{a} ({ap}) approved"),
        color = "blue",
        icon = icon("vial")
        # info = ""
      )
    })
    
    output$n_countries <- renderValueBox({
      valueBoxSpark(
        width = 12,
        title = "Countries",
        value = scales::number(df_summary()$n_countries),
        subtitle = glue::glue("{scales::number(df_summary()$n_regions)} WHO region(s)"),
        color = "teal",
        icon = icon("globe-africa")
        # info = "
      )
    })
    
    output$time_decision <- renderValueBox({
      valueBoxSpark(
        width = 12,
        title = "Average decision time",
        value = glue::glue("{df_summary()$time_decision_av} days"),
        subtitle = glue::glue("Min: {df_summary()$time_decision_min} days - Max: {df_summary()$time_decision_max} days"),
        color = "green",
        icon = icon("clock")
        # info = "
      )
    })
    
    # ==========================================================================
    # MAP
    # ==========================================================================
    
    output$map <- leaflet::renderLeaflet({
      bbox <- c(xmin = -180, ymin = -55.61183, xmax = 180, ymax = 83.64513)
      
      leaflet::leaflet() %>%
        leaflet::setView(lng = 11, lat = 4.64916, zoom = 3) %>% 
        # leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) %>%
        leaflet::addMapPane(name = "choropleth", zIndex = 300) %>%
        leaflet::addMapPane(name = "circles", zIndex = 420) %>%
        leaflet::addMapPane(name = "geo_highlight", zIndex = 430) %>%
        leaflet::addMapPane(name = "place_labels", zIndex = 440) %>%
        # leaflet::addProviderTiles("CartoDB.PositronNoLabels", group = "Light") %>%
        # leaflet::addProviderTiles("CartoDB.PositronOnlyLabels", group = "Labels", options = leaflet::leafletOptions(pane = "place_labels")) %>%
        leaflet::addProviderTiles("OpenStreetMap", group = "OSM") %>%
        # leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM HOT") %>%
        leaflet::addScaleBar(position = "bottomleft") %>%
        leaflet.extras::addFullscreenControl(position = "topleft") %>%
        leaflet.extras::addResetMapButton() %>% 
        # leaflet::addLayersControl(
        #   # baseGroups = c("OSM", "OSM HOT", "Light"),
        #   overlayGroups = c("Labels"),
        #   position = "topleft"
        # ) %>% 
        leaflet::addPolygons(
          data = sf_world,
          stroke = TRUE,
          color = "grey",
          weight = 1,
          fillOpacity = 0,
          label = ~country,
          layerId = ~iso_a3,
          group = "Boundaries",
          options = leaflet::pathOptions(pane = "choropleth")
        ) %>%
        addMinicharts(
          sf_world$lon, 
          sf_world$lat,
          layerId = sf_world$country,
          chartdata = 1,
          width = 0, 
          height = 0
        )
    })
    
    # Region select observers ====================================================
    
    # reactive val boolean to indicate if a shape has been selected
    map_click <- reactiveVal(FALSE)
    country_select <- reactiveVal(NULL)
    
    # if country is selected from map, update country_select value
    observeEvent(input$map_shape_click$id, {
      # browser()
      iso <- input$map_shape_click$id
      map_click(TRUE)
      country_select(iso)
      # shp <- sf_world %>% filter(iso_a3 == iso)
      # leaflet::leafletProxy("map", session) %>%
      #   leaflet::addPolylines(
      #     data = shp,
      #     layerId = "highlight",
      #     stroke = TRUE,
      #     opacity = 1,
      #     weight = 2,
      #     color = "red",
      #     options = leaflet::pathOptions(pane = "geo_highlight")
      #   )
    })
    
    observeEvent(input$map_click, {
      if (map_click()) {
        map_click(FALSE)
        # country_select(NULL)
        # leaflet::leafletProxy("map", session) %>% leaflet::removeShape("highlight")
      }
    })
    
    observeEvent(input$var, {
      cond <- (input$var == "Doses")
      shinyjs::toggle("dose", condition = cond, anim = TRUE, animType = "fade")
    })
    
    observe({
      ts_date_selected <- input$ts_date
      if (input$var == "Doses" & input$dose == "n_dose_ship") {
        shinyWidgets::updatePickerInput(
          session,
          "ts_date",
          choices = date_vars,
          selected = ts_date_selected
        )
      } else {
        shinyWidgets::updatePickerInput(
          session,
          "ts_date",
          choices = date_vars[1:2],
          selected = ts_date_selected
        )
      }
    })
    
    df_map <- reactive({
      map_var <- rlang::sym(input$var)
      map_group <- rlang::sym(input$group)
      
      if (input$var == "Requests") {
        
        df_counts <- df_data() %>% 
          drop_na(!!map_group) %>%
          mutate(!!map_group := forcats::fct_infreq(!!map_group)) %>%
          janitor::tabyl(iso_a3, !!map_group) %>%
          janitor::adorn_totals("col", name = "total")
        
        df_map <- sf_world %>% 
          sf::st_drop_geometry() %>% 
          select(country, iso_a3, lon, lat) %>% 
          left_join(df_counts, by = "iso_a3") %>% 
          mutate(across(where(is.numeric), as.double)) %>% 
          mutate(across(where(is.double), ~if_else(is.na(.x), 0, .x)))
        
      } else if (input$var == "Doses") {
        map_dose <- rlang::sym(input$dose)
        
        df_counts <- df_data() %>% 
          drop_na(!!map_group) %>%
          count(iso_a3, !!map_group, wt = !!map_dose) %>% 
          mutate(!!map_group := forcats::fct_reorder(!!map_group, n, .desc = T)) %>%
          add_count(iso_a3, wt = n, name = "total") %>% 
          pivot_wider(names_from = input$group, values_from = "n", values_fill = 0)
        
        df_map <- sf_world %>% 
          sf::st_drop_geometry() %>% 
          select(country, iso_a3, lon, lat) %>% 
          left_join(df_counts, by = "iso_a3") %>% 
          mutate(across(where(is.numeric), as.double)) %>% 
          mutate(across(where(is.double), ~if_else(is.na(.x), 0, .x)))
      }
      
      return(df_map)
    })
    
    observe({
      df_map <- df_map()
      chartData <- df_map %>% 
        select(any_of(grouping_levels))
      #select(-country, -iso_a3, -lon, -lat, -total)
      pie_width <- 60 * sqrt(df_map$total) / sqrt(max(df_map$total))
      
      leaflet::leafletProxy("map", session) %>%
        updateMinicharts(
          layerId = df_map$country,
          chartdata = chartData,
          width = pie_width,
          opacity = .8,
          legend = TRUE,
          showLabels = TRUE,
          type = "pie"
        )
    })
    
    # ==========================================================================
    # TIME-SERIES
    # ==========================================================================
    
    # observeEvent(input$var, {
    #   cond <- (input$var == "Doses")
    #   shinyjs::toggle("ts_dose", condition = cond, anim = TRUE, animType = "fade")
    # })
    
    df_ts <- reactive({
      req(input$ts_date, cancelOutput = TRUE)
      
      if (input$var == "Doses" & input$ts_date == "date_delivery") {
        df <- df_shipment %>% 
          inner_join(
            df_data() %>% distinct(id_demand, request_mechanism, request_status), 
            by = "id_demand"
          )
      } else {
        df <- df_data()
      }
      # if (!is.null(country_select())) df %<>% filter(iso_a3 == country_select())
      
      req(input$ts_date %in% names(df), cancelOutput = TRUE)
      
      ts_var <- rlang::sym(input$var)
      ts_group <- rlang::sym(input$group)
      ts_date <- rlang::sym(input$ts_date)
      
      g_levels <- if (input$group == "request_mechanism") {
        grouping_levels[1:3]
      } else if (input$group == "request_status") {
        grouping_levels[4:7]
      }

      if (input$var == "Requests") {
        df_counts <- df %>% 
          mutate(time_unit = as_date(floor_date(!!ts_date, unit = input$ts_unit))) %>% 
          # mutate(!!ts_group := forcats::fct_infreq(!!ts_group) %>% forcats::fct_explicit_na("Unknown")) %>%
          mutate(!!ts_group := factor(!!ts_group, levels = g_levels) %>% forcats::fct_explicit_na("Unknown")) %>%
          count(time_unit, !!ts_group) %>% 
          arrange(time_unit)
        
      } else if (input$var == "Doses") {
        ts_dose <- rlang::sym(input$dose)
        
        df_counts <- df %>% 
          mutate(time_unit = as_date(floor_date(!!ts_date, unit = input$ts_unit))) %>% 
          count(time_unit, !!ts_group, wt = !!ts_dose) %>% 
          mutate(!!ts_group := factor(!!ts_group, levels = g_levels) %>% forcats::fct_explicit_na("Unknown"))
          # mutate(!!ts_group := forcats::fct_reorder(!!ts_group, n, .desc = T) %>% forcats::fct_explicit_na("Unknown"))
      }
      
      return(df_counts)
    })
    
    output$ts_chart <- renderHighchart({
      req(df_ts())
      
      ts_group <- rlang::sym(input$group)
      df_ts <- df_ts() %>% drop_na(time_unit) 
      
      if (isolate(input$ts_unit) == "quarter") {
        q_range <- range(df_ts$time_unit)
        complete_quarters <- seq.Date(q_range[1], q_range[2], by = "3 months") 
        df_ts %<>% 
          arrange(time_unit) %>% 
          complete(time_unit = complete_quarters, !!ts_group, fill = list(n = 0)) %>% 
          mutate(
            time_lab = quarter(time_unit, with_year = TRUE) %>% str_replace("\\.", "-Q") %>% factor(),
            time_unit = as.numeric(time_lab)
          )
        hc <- hchart(df_ts, "column", hcaes(x = time_unit, y = n, group = !!ts_group, name = time_lab))
        x_type <- "category"
      } else {
        hc <- hchart(df_ts, "column", hcaes(x = time_unit, y = n, group = !!ts_group))
        x_type <- "datetime"
      }
      
      date_lab <- names(date_vars[date_vars == isolate(input$ts_date)])
      
      hc %>% 
        hc_title(text = NULL) %>%
        hc_chart(zoomType = "x") %>%
        hc_xAxis(type = x_type, title = list(text = date_lab), crosshair = TRUE) %>% 
        highcharter::hc_yAxis_multiples(
          list(
            title = list(text = ""),
            allowDecimals = FALSE
          ),
          list(
            title = list(text = ""),
            allowDecimals = FALSE,
            opposite = TRUE,
            linkedTo = 0
          )
        ) %>%
        hc_tooltip(shared = TRUE) %>% 
        hc_credits(enabled = FALSE) %>%
        hc_legend(
          title = list(text = ""),
          layout = "vertical",
          align = "right",
          verticalAlign = "top",
          x = -10,
          y = 40
        ) %>% 
        my_hc_export()
    })
    
    # ==========================================================================
    # DELAYS
    # ==========================================================================
    
    observe({
      cond <- (input$delay_tabs == "chart")
      shinyjs::toggle("delay_stacking", condition = cond, anim = TRUE, animType = "fade")
      shinyjs::toggle("delay_log", condition = cond, anim = TRUE, animType = "fade")
    })
    
    df_delay <- reactive({
      range <- c(input$date_1, input$date_2)
      date_1 <- rlang::sym(range[1])
      date_2 <- rlang::sym(range[2])
      
      df_delay <- app_data$df_delay %>% 
        # filter to requests in pre-filtered df_data
        semi_join(df_data(), by = "id_demand") %>% 
        select(id_demand, request_mechanism, request_status, event, date) %>% 
        filter(event %in% range) %>% 
        pivot_wider(names_from = "event", values_from = "date") %>% 
        mutate(delay = as.numeric({{ date_2 }} - {{ date_1 }}))
    })
    
    output$delay <- renderHighchart({
      
      df_delay <- df_delay()
      delay_group <- rlang::sym(input$group)
      delay_mean <- mean(df_delay$delay, na.rm = TRUE)
      delay_median <- median(df_delay$delay, na.rm = TRUE)
      n_missing <- sum(is.na(df_delay$delay))
      
      df_hc <- df_delay %>% 
        drop_na(delay) %>% 
        count(!!delay_group, delay) %>% 
        mutate(!!delay_group := factor(!!delay_group, levels = grouping_levels) %>% forcats::fct_explicit_na("Unknown"))
      
      stacking <- input$delay_stacking
      if (input$delay_stacking == "none") {
        stacking <- NULL
      }
        
      hchart(df_hc, "column", hcaes(delay, n, group = !!delay_group)) %>%
        hc_chart(zoomType = "x") %>%
        hc_title(text = NULL) %>%
        hc_xAxis(
          title = list(text = "Days"),
          allowDecimals = FALSE,
          crosshair = TRUE,
          min = 0,
          plotLines = list(
            list(
              color = "red", zIndex = 1, value = delay_mean,
              label = list(text = "Mean", verticalAlign = "top", textAlign = "left")
            ),
            list(
              color = "red", zIndex = 1, value = delay_median,
              label = list(text = "Median", verticalAlign = "top", textAlign = "left")
            )
          )
        ) %>%
        hc_yAxis(title = list(text = ""), allowDecimals = FALSE) %>%
        hc_plotOptions(column = list(stacking = stacking)) %>% 
        hc_tooltip(shared = TRUE) %>%
        hc_legend(
          title = list(text = ""),
          layout = "vertical",
          align = "right",
          verticalAlign = "top",
          x = -10,
          y = 40
        ) %>% 
        hc_credits(enabled = TRUE, text = glue::glue("Unknown delay time for {scales::number(n_missing)} cases")) %>%
        my_hc_export()
    })
    
    output$delay_tbl <- gt::render_gt({
      df <- df_delay()
      group <- input$group
      group_lab <- dplyr::if_else(group == "request_mechanism", "Mechanism", "Status")
      
      df %>% 
        dplyr::select(.data[[group]], delay) %>% 
        gtsummary::tbl_summary(
          by = input$group,
          label = list(delay ~ "Days between events"),
          type = gtsummary::all_continuous() ~ "continuous2",
          digits = list(delay ~ c(0, 2, 2, 0, 0, 0, 0, 0)),
          statistic = gtsummary::all_continuous() ~ c("{N_nonmiss}", 
                                                      "{mean} ({sd})", 
                                                      "{median} ({p25}, {p75})", 
                                                      "{min}, {max}")
        ) %>%
        gtsummary::modify_header(update = gtsummary::all_stat_cols() ~ "**{level}**") %>%
        gtsummary::add_overall(col_label = glue::glue("**{group_lab}**")) %>%
        gtsummary::italicize_levels() %>%
        gtsummary::modify_footnote(update = gtsummary::everything() ~ NA) %>%
        gtsummary::as_gt()
      
    })
    
    # ==========================================================================
    # TIMEVIS
    # ==========================================================================
    
    observe({
      df_demands <- df_data() %>% distinct(request_country, id_demand) %>% arrange(request_country, desc(id_demand))
      demands <- split(df_demands$id_demand, df_demands$request_country)
      # browser()
      shinyWidgets::updatePickerInput(session, "demand", choices = demands)
    })
    
    output$timevis <- renderTimevis({
      req(input$demand)
      
      df_out <- df_timevis %>% 
        filter(id_demand == input$demand) %>% 
        drop_na(start)
      
      dates <- unique(df_out$start)
      
      start_date <- min(dates) - lubridate::days(3)
      end_date <- max(dates) + lubridate::days(3)
      
      timevis(
        df_out,
        options = list(
          start = start_date, 
          end = end_date,
          fit = FALSE
        )
      )
    })
    
  })
}
