
mod_request_ui <- function(id) {
  ns <- NS(id)
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
        shinydashboard::valueBoxOutput(ns("n_requests"), width = 4),
        shinydashboard::valueBoxOutput(ns("n_doses"), width = 4),
        shinydashboard::valueBoxOutput(ns("n_countries"), width = 4)
      ),
      
      fluidRow(
        box_w_inputs(
          width = 12,
          title = tagList(shiny::icon("globe-africa")),
          inputs = tagList(
            shinyWidgets::radioGroupButtons(
              ns("map_var"),
              label = NULL,
              choices = c("Requests", "Doses"),
              size = "sm"
            ),
            shinyWidgets::radioGroupButtons(
              ns("map_group"),
              label = NULL,
              choices = c("Mechanism" = "request_mechanism", "Status" = "request_status"),
              size = "sm"
            ),
            shinyWidgets::radioGroupButtons(
              ns("map_dose"),
              label = NULL,
              choices = c("Requested" = "n_dose_request", "Approved" = "n_dose_approve"),
              size = "sm"
            )
          ),
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
          width = 12,
          title = tagList(shiny::icon("bar-chart")),
          inputs = tagList(
            shinyWidgets::radioGroupButtons(
              ns("ts_unit"),
              label = NULL,
              choices = c("Week" = "week", "Month" = "month", "Year" = "year"),
              selected = "year",
              size = "sm"
            ),
            shinyWidgets::radioGroupButtons(
              ns("ts_var"),
              label = NULL,
              choices = c("Requests", "Doses"),
              size = "sm"
            ),
            shinyWidgets::radioGroupButtons(
              ns("ts_group"),
              label = NULL,
              choices = c("Mechanism" = "request_mechanism", "Status" = "request_status"),
              size = "sm"
            ),
            shinyWidgets::radioGroupButtons(
              ns("ts_dose"),
              label = NULL,
              choices = c("Requested" = "n_dose_request", "Approved" = "n_dose_approve"),
              size = "sm"
            )
          ),
          highcharter::highchartOutput(ns("ts_chart")),
          footer = uiOutput(ns("cfr_footer"))
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
    
    geo_select <- reactiveVal("World")
    observeEvent(input$geo, { geo_select(input$geo) })
    
    df_data <- reactive({
      df <- df_request %>% 
        filter(quarter >= input$q_range[1], quarter <= input$q_range[2])
      
      if (length(input$geo)) df %<>% filter_geo(input$geo)
      if (length(input$status)) df %<>% filter(request_status %in% input$status)
      if (length(input$context)) df %<>% filter(context %in% input$context)
      if (length(input$mechanism)) df %<>% filter(request_mechanism %in% input$mechanism)
      if (length(input$agency)) df %<>% filter(request_agency %in% input$agency)
      # if (length(input$vaccin)) df %<>% filter(sc_ocv_recu %in% input$vaccin)
      
      return(df)
    }) %>% bindEvent(input$go1, input$go2, ignoreInit = FALSE, ignoreNULL = FALSE)
    
    # ==========================================================================
    # VALUE BOXES
    # ==========================================================================
    
    df_summary <- reactive({
      df_data() %>% 
        summarise(
          n_requests = n(),
          n_approved = sum(request_status == "Approved", na.rm = TRUE),
          pcnt_approved = n_approved / n_requests,
          n_dose_requested = sum(n_dose_request, na.rm = TRUE),
          n_dose_approved = sum(n_dose_approve, na.rm = TRUE),
          pcnt_dose_approved = n_dose_approved / n_dose_requested,
          n_regions = n_distinct(who_region, na.rm = TRUE),
          n_countries = n_distinct(request_country, na.rm = TRUE)
        )
    })
    
    output$n_requests <- renderValueBox({
      r <-scales::number(df_summary()$n_requests)
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
      r <-scales::number(df_summary()$n_dose_requested)
      a <- scales::number(df_summary()$n_dose_approved)
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
        leaflet::addMapPane(name = "district_highlight", zIndex = 430) %>%
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
      map_click(TRUE)
      country_select(input$map_shape_click$id)
    })
    
    observeEvent(input$map_click, {
      if (map_click()) {
        map_click(FALSE)
        country_select(NULL)
      }
    })
    
    observeEvent(input$map_var, {
      cond <- (input$map_var == "Doses")
      shinyjs::toggle("map_dose", condition = cond, anim = TRUE, animType = "fade")
    })
    
    df_map <- reactive({
      map_var <- rlang::sym(input$map_var)
      map_group <- rlang::sym(input$map_group)
      
      if (input$map_var == "Requests") {
        
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
        
      } else if (input$map_var == "Doses") {
        map_dose <- rlang::sym(input$map_dose)
        
        df_counts <- df_data() %>% 
          drop_na(!!map_group) %>%
          count(iso_a3, !!map_group, wt = !!map_dose) %>% 
          mutate(!!map_group := forcats::fct_reorder(!!map_group, n, .desc = T)) %>%
          add_count(iso_a3, wt = n, name = "total") %>% 
          pivot_wider(names_from = input$map_group, values_from = "n", values_fill = 0)
        
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
        select(any_of(c("ICG", "GTFCC", "Loan", "Approved", "Not approved", "Pending", "Cancelled")))
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
    
    #############################################
    # Time-series
    #############################################
    
    observeEvent(input$ts_var, {
      cond <- (input$ts_var == "Doses")
      shinyjs::toggle("ts_dose", condition = cond, anim = TRUE, animType = "fade")
    })
    
    df_ts <- reactive({
      ts_var <- rlang::sym(input$ts_var)
      ts_group <- rlang::sym(input$ts_group)
      
      if (input$ts_var == "Requests") {
        
        df_counts <- df_data() %>% 
          mutate(time_unit = as_date(floor_date(date_receipt, unit = input$ts_unit))) %>% 
          mutate(!!ts_group := forcats::fct_infreq(!!ts_group) %>% forcats::fct_explicit_na("Unknown")) %>%
          count(time_unit, !!ts_group) %>% 
          arrange(time_unit)
        
      } else if (input$ts_var == "Doses") {
        ts_dose <- rlang::sym(input$ts_dose)
        
        df_counts <- df_data() %>% 
          mutate(time_unit = as_date(floor_date(date_decision, unit = input$ts_unit))) %>% 
          count(time_unit, !!ts_group, wt = !!ts_dose) %>% 
          mutate(!!ts_group := forcats::fct_reorder(!!ts_group, n, .desc = T) %>% forcats::fct_explicit_na("Unknown"))
      }
      
      return(df_counts)
    })
    
   output$ts_chart <- renderHighchart({
     ts_group <- rlang::sym(input$ts_group)
     
     # browser()
     
     hchart(df_ts() %>% drop_na(time_unit), "column", hcaes(x = time_unit, y = n, group = !!ts_group)) %>% 
       hc_title(text = NULL) %>%
       hc_chart(zoomType = "x") %>%
       hc_xAxis(title = list(text = ""), crosshair = TRUE) %>% 
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
    
    
    
  })
}
