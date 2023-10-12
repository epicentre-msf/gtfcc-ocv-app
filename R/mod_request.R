
mod_request_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    border = FALSE,
    fillable = FALSE,
    sidebar = sidebar(
      div(
        
        id = ns("resetable_filters"),
        shinyWidgets::sliderTextInput(
          inputId = ns("q_range"),
          label = "Quarter of request",
          choices = q_range,
          selected = c(min(q_range), max(q_range)),
          grid = FALSE,
          animate = FALSE,
          width = "95%"
        ),
        
        shinyWidgets::pickerInput(
          inputId = ns("region"),
          label = "Region",
          choices = unique(df_request$r_who_region) %>% na.omit(),
          options = picker_opts(),
          multiple = TRUE
        ),
        
        shinyWidgets::pickerInput(
          inputId = ns("country"),
          label = "Country",
          choices = unique(df_request$r_country) %>% na.omit(),
          options = picker_opts(search = TRUE),
          multiple = TRUE
        ),
        
        shinyWidgets::pickerInput(
          inputId = ns("status"),
          label = "Status",
          choices = unique(df_request$r_status) %>% na.omit(),
          options = picker_opts(),
          multiple = TRUE
        ),
        
        shinyWidgets::pickerInput(
          inputId = ns("context"),
          label = "Context",
          choices = unique(df_request$r_context) %>% na.omit(),
          options = picker_opts(),
          multiple = TRUE
        ),
        
        shinyWidgets::pickerInput(
          inputId = ns("mechanism"),
          label = "Mechanism",
          choices = unique(df_request$r_mechanism_type) %>% na.omit(),
          options = picker_opts(),
          multiple = TRUE
        ),
        
        shinyWidgets::pickerInput(
          inputId = ns("agency"),
          label = "Request Agency",
          choices = unique(df_request$r_agency) %>% na.omit(),
          options = picker_opts(),
          multiple = TRUE
        ),
        
        shinyWidgets::pickerInput(
          inputId = ns("vaccine"),
          label = "Type of vaccine",
          choices = unique(df_shipment$s_vaccine) %>% na.omit(),
          options = picker_opts(),
          multiple = TRUE
        ),
        
        actionButton(ns("go"), "Update Data", icon = icon("database"), class = "btn-success btn-sm", style = "color: #fff;"),
        actionButton(ns("reset"), "Reset Inputs", icon = icon("arrows-rotate"), class = "btn-warning btn-sm", style = "color: #fff;")
      )
    ),
    uiOutput(ns("value_boxes")),
    tags$hr(),
    div(
      class = "row",
      div(
        class = "col-auto",
        shinyWidgets::radioGroupButtons(
          ns("var"),
          label = NULL,
          choices = c("Requests", "Doses"),
          size = "sm",
          status = "outline-success",
          justified = FALSE
        )
      ),
      div(
        class = "col-auto",
        shinyWidgets::radioGroupButtons(
          ns("group"),
          label = NULL,
          choices = group_vars,
          size = "sm",
          status = "outline-success",
          justified = FALSE
        )
      ),
      div(
        class = "col-auto",
        shinyWidgets::radioGroupButtons(
          ns("dose"),
          label = NULL,
          choices = dose_vars,
          status = "outline-success",
          size = "sm",
          justified = FALSE
        )
      )
    ),

    # Geo tabs ==============================================
    navset_card_tab(
      full_screen = TRUE,
      wrapper = \(...) {
        bslib::card_body(..., padding = 0)
      },
      title = div(
        class = "d-flex justify-content-between align-items-center",
        uiOutput(ns("map_title")),
        shinyscreenshot::screenshotButton(
          id = ns("map"),
          filename = glue::glue("GTFCC-Map-{Sys.Date()}"),
          label = "Download",
          class = "btn-outline-success btn-sm pe-2"
        )
      ),
      nav_panel(
        title = shiny::icon("globe-africa"),
        value = "map",
        leaflet::leafletOutput(ns("map"))
      ),
      nav_panel(
        title = shiny::icon("chart-column"),
        value = "chart",
        highcharter::highchartOutput(ns("map_chart"))
      ),
      nav_panel(
        title = shiny::icon("table"),
        value = "table",
        reactable::reactableOutput(ns("map_tbl"))
      )
    ),
    layout_column_wrap(
      width = "500px",

      # Time series ==============================================
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex align-items-center",
          tags$span(class = "me-auto pe-1", shiny::icon("chart-column"), "Time-series"),
          
          div(class = "pe-1", shinyWidgets::radioGroupButtons(
            ns("ts_unit"),
            label = NULL,
            choices = c("Year" = "year", "Quarter" = "quarter", "Month" = "month", "Week" = "week"),
            selected = "year",
            size = "sm",
            status = "outline-success"
          )),
          
          div(class = "pe-1", shinyWidgets::pickerInput(
            ns("ts_date"),
            label = NULL,
            choices = date_vars[1:2],
            selected = date_vars[1],
            options = picker_opts(actions = FALSE, search = FALSE),
            width = 150,
            multiple = FALSE
          ))

          # class = "d-flex justify-content-between align-items-center",
          # tags$span(
          #   shiny::icon("chart-column"),
          #   "Time-series",
          #   bslib::tooltip(
          #     bs_icon("info-circle"),
          #     "Click the gear icon on the right for chart options."
          #   )
          # ),
          # popover(
          #   bs_icon("gear"),
          #   title = "Time-series inputs",
          #   placement = "left",
          #   shinyWidgets::radioGroupButtons(
          #     ns("ts_unit"),
          #     label = "Time interval",
          #     choices = c("Year" = "year", "Quarter" = "quarter", "Month" = "month", "Week" = "week"),
          #     selected = "year",
          #     size = "sm",
          #     status = "outline-success"
          #   ),
          #   shinyWidgets::pickerInput(
          #     ns("ts_date"),
          #     label = "Date variable",
          #     choices = date_vars[1:2],
          #     selected = date_vars[1],
          #     options = picker_opts(actions = FALSE, search = FALSE),
          #     width = "100%",
          #     multiple = FALSE
          #   )
          # )
        ),
        card_body(
          padding = 0,
          highcharter::highchartOutput(ns("ts_chart"))
        )
      ),

      # Delay tabs ==============================================
      navset_card_tab(
        full_screen = TRUE,
        wrapper = \(...) {
          bslib::card_body(..., padding = 0)
        },
        id = ns("delay_tabs"),
        title = div(
          class = "d-flex justify-content-between align-items-center",
          tags$span(
            class = "pe-2",
            tagList(shiny::icon("clock"), "Delays")
          ),
          div(class = "pe-2", shinyWidgets::pickerInput(
            ns("delay_var"),
            label = NULL,
            choices = purrr::set_names(delay_vars$var, delay_vars$lab),
            options = picker_opts(actions = FALSE, search = FALSE),
            width = 150,
            multiple = FALSE
          )),
          popover(
            bs_icon("gear"),
            title = "Time interval",
            placement = "left",
            shinyWidgets::radioGroupButtons(
              ns("delay_unit"),
              label = NULL,
              choices = c("Year" = "year", "Quarter" = "quarter", "Month" = "month", "Week" = "week"),
              selected = "year",
              size = "sm",
              status = "outline-success"
            )
          )

          # div(class="pe-1", shinyWidgets::pickerInput(
          #   ns("date_1"),
          #   label = NULL,
          #   choices = delay_choices,
          #   selected = delay_choices[1],
          #   options = picker_opts(actions = FALSE, search = FALSE),
          #   width = 80,
          #   multiple = FALSE
          # )),
          # helpText("-"),
          # div(class="pe-1", shinyWidgets::pickerInput(
          #   ns("date_2"),
          #   label = NULL,
          #   choices = delay_choices,
          #   selected = delay_choices[2],
          #   options = picker_opts(actions = FALSE, search = FALSE),
          #   width = 80,
          #   multiple = FALSE
          # )),
          # div(class="pe-1", shinyWidgets::radioGroupButtons(
          #   ns("delay_stacking"),
          #   label = NULL,
          #   choices = c("Stacked bars" = "normal", "Dodged bars" = "none"),
          #   size = "sm",
          #   status = "outline-success"
          # ))
        ),
        nav_panel(
          title = shiny::icon("chart-line"),
          value = "boxplot",
          highcharter::highchartOutput(ns("delay_boxplot"))
        ),
        nav_panel(
          title = shiny::icon("chart-column"),
          value = "chart",
          highcharter::highchartOutput(ns("delay_hist"))
        ),
        nav_panel(
          title = shiny::icon("table"),
          value = "table",
          gt::gt_output(ns("delay_tbl"))
        )

        # footer = card_footer("Delays are calculated on ICG data only.")
      )
    ),

    # Request timeline ==============================================
    card(
      class = "my-3",
      card_header(
        class = "d-flex mb-0 align-items-center",
        tags$span(
          class = "pe-2",
          bsicons::bs_icon("calendar-week"),
          "Request Timeline",
          tags$small(" (select one)")
        ),
        shinyWidgets::pickerInput(
          inputId = ns("demand"),
          label = NULL,
          choices = "",
          width = 150,
          options = picker_opts(search = TRUE),
          multiple = FALSE
        )
      ),
      card_body(
        timevis::timevisOutput(ns("timevis"))
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
    # OBSERVERS
    # ==========================================================================

    observeEvent(input$reset, {
      shinyjs::reset("resetable_filters")
    })

    observeEvent(input$var, {
      cond <- (input$var == "Doses")
      shinyjs::toggle("dose", condition = cond, anim = TRUE, animType = "fade")
    })

    observe({
      ts_date_selected <- isolate(input$ts_date)
      if (input$var == "Doses" & input$dose == "s_dose_ship") {
        shinyWidgets::updatePickerInput(
          session,
          "ts_date",
          choices = date_vars,
          selected = "s_date_delivery"
        )
      } else if (ts_date_selected %in% date_vars[1:2]) {
        shinyWidgets::updatePickerInput(
          session,
          "ts_date",
          choices = date_vars[1:2],
          selected = ts_date_selected
        )
      } else {
        shinyWidgets::updatePickerInput(
          session,
          "ts_date",
          choices = date_vars[1:2],
          selected = date_vars[1]
        )
      }
    })

    var_lab <- reactive({
      lab <- paste("Number of", tolower(input$var))
      if (input$var == "Doses") {
        lab <- paste(lab, tolower(names(dose_vars[dose_vars == input$dose])))
      }
      lab
    })

    output$map_title <- renderUI({
      tags$span(
        class = "pe-2",
        shiny::icon("earth-africa"),
        stringr::str_replace(var_lab(), "Number", "Map")
      )
    })

    # ==========================================================================
    # DATA PREP
    # ==========================================================================

    observe({
      if (length(input$region)) {
        countries <- df_request %>%
          filter(r_who_region %in% input$region) %>%
          distinct(r_country) %>%
          pull() %>%
          sort()
        selected <- intersect(input$country, countries)
        shinyWidgets::updatePickerInput(session, "country", choices = countries, selected = selected)
      } else {
        countries <- unique(df_request$r_country) %>%
          na.omit() %>%
          sort()
        selected <- intersect(input$country, countries)
        shinyWidgets::updatePickerInput(session, "country", choices = countries, selected = selected)
      }
    })

    geo_select <- reactiveVal("World")
    observeEvent(input$geo, {
      geo_select(input$geo)
    })

    df_data <- reactive({
      df <- df_request %>%
        filter(quarter >= input$q_range[1], quarter <= input$q_range[2])

      # if (length(input$geo)) df %<>% filter_geo(input$geo)
      if (length(input$region)) df %<>% filter(r_who_region %in% input$region)
      if (length(input$country)) df %<>% filter(r_country %in% input$country)
      if (length(input$status)) df %<>% filter(r_status %in% input$status)
      if (length(input$context)) df %<>% filter(r_context %in% input$context)
      if (length(input$mechanism)) df %<>% filter(r_mechanism_type %in% input$mechanism)
      if (length(input$agency)) df %<>% filter(r_agency %in% input$agency)
      # if (!is.null(country_select())) df %<>% filter(iso_a3 == country_select())
      if (length(input$vaccine)) {
        vacc_filter <- df_shipment %>%
          filter(s_vaccine %in% input$vaccine) %>%
          distinct(r_demand_id = s_r_demand_id)
        df %<>% semi_join(vacc_filter, by = "r_demand_id")
      }

      return(df)
    }) %>% bindEvent(input$go, ignoreInit = FALSE, ignoreNULL = FALSE)

    # ==========================================================================
    # VALUE BOXES
    # ==========================================================================

    df_summary <- reactive({
      df <- df_data()
      # if (!is.null(country_select())) df %<>% filter(iso_a3 == country_select())
      df %>%
        mutate(time_decision = r_date_decision - r_date_receipt) %>%
        summarise(
          n_requests = n(),
          n_approved = sum(r_status == "Approved", na.rm = TRUE),
          pcnt_approved = n_approved / n_requests,
          n_dose_requested = sum(r_dose_request, na.rm = TRUE),
          n_dose_approved = sum(r_dose_approve, na.rm = TRUE),
          pcnt_dose_approved = n_dose_approved / n_dose_requested,
          n_dose_shipped = sum(s_dose_ship, na.rm = TRUE),
          n_dose_shipped_icg = sum(s_dose_ship[r_mechanism == "ICG"], na.rm = TRUE),
          pcnt_dose_shipped_icg = n_dose_shipped_icg / n_dose_shipped,
          n_regions = n_distinct(r_who_region, na.rm = TRUE),
          n_countries = n_distinct(r_country, na.rm = TRUE),
          n_countries_approved = n_distinct(r_country[r_status == "Approved"], na.rm = TRUE),
          pcnt_countries_approved = n_countries_approved / n_countries,
          time_decision_av = round(mean(time_decision, na.rm = TRUE), 1),
          time_decision_min = min(time_decision, na.rm = TRUE),
          time_decision_max = max(time_decision, na.rm = TRUE)
        )
    })

    output$value_boxes <- renderUI({
      # requests
      r <- scales::number(df_summary()$n_requests)
      a <- scales::number(df_summary()$n_approved)
      ap <- scales::percent(df_summary()$pcnt_approved)
      n_requests <- value_box(
        title = "Requests received",
        value = r,
        tags$p(glue::glue("{a} ({ap}) approved")),
        showcase = bsicons::bs_icon("card-checklist"),
        theme = "primary"
        # showcase_layout = showcase_top_right()
      )
      # doses
      r <- fmt_n_dose(df_summary()$n_dose_requested)
      a <- fmt_n_dose(df_summary()$n_dose_approved)
      ap <- scales::percent(df_summary()$pcnt_dose_approved)
      n_doses <- value_box(
        title = "Doses requested",
        value = r,
        tags$p(glue::glue("{a} ({ap}) approved")),
        showcase = bsicons::bs_icon("box-seam"),
        theme = "info"
        # showcase_layout = showcase_top_right()
      )
      # doses shipped
      s <- fmt_n_dose(df_summary()$n_dose_shipped)
      s_icg <- fmt_n_dose(df_summary()$n_dose_shipped_icg)
      s_icg_p <- scales::percent(df_summary()$pcnt_dose_shipped_icg)
      n_doses_shipped <- value_box(
        title = "Doses shipped",
        value = s,
        tags$p(glue::glue("{s_icg} ({s_icg_p}) shipped with ICG")),
        theme = "success",
        showcase = bsicons::bs_icon("airplane")
        # showcase_layout = showcase_top_right()
      )
      # n countries
      n <- scales::number(df_summary()$n_countries)
      na <- scales::number(df_summary()$n_countries_approved)
      pcnt <- scales::percent(df_summary()$pcnt_countries_approved)
      n_countries <- value_box(
        title = "Countries",
        value = n,
        tags$p(glue::glue("{na} ({pcnt}) with an approved request")),
        theme = "warning",
        showcase = bsicons::bs_icon("globe")
        # showcase_layout = showcase_top_right()
      )
      # time decision
      days <- as.character(glue::glue("{df_summary()$time_decision_av} days"))
      # days <- df_summary()$time_decision_av
      time_decision <- value_box(
        title = "Average decision time",
        value = days,
        tags$p(glue::glue("Min: {df_summary()$time_decision_min} days - Max: {df_summary()$time_decision_max} days")),
        theme = "danger",
        showcase = bsicons::bs_icon("clock")
        # showcase_layout = showcase_top_right()
      )

      layout_column_wrap(
        width = "200px",
        class = "mb-3",
        n_requests,
        n_doses,
        n_doses_shipped,
        n_countries,
        time_decision
      )
    })

    # ==========================================================================
    # MAP
    # ==========================================================================

    # MAP

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
          mutate(across(where(is.double), ~ if_else(is.na(.x), 0, .x)))
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
          mutate(across(where(is.double), ~ if_else(is.na(.x), 0, .x)))
      }

      return(df_map)
    })

    observe({
      df_map <- df_map()
      chartData <- df_map %>%
        select(any_of(grouping_levels))
      # select(-country, -iso_a3, -lon, -lat, -total)
      pie_width <- 45 * sqrt(df_map$total) / sqrt(max(df_map$total))

      mp <- map_pal(chartData)

      leaflet::leafletProxy("map", session) %>%
        updateMinicharts(
          layerId = df_map$country,
          chartdata = chartData,
          width = pie_width,
          colorPalette = mp,
          opacity = .8,
          legend = TRUE,
          showLabels = TRUE,
          type = "pie"
        )
    })

    # MAP_CHART
    # make df for the highcarter barplot

    df_map_chart <- reactive({
      map_chart_group <- rlang::sym(input$group)
      map_chart_dose_vars <- rlang::sym(input$dose)

      # use function to prepare the data
      dat <- df_hc_bar(
        df_data = df_data(),
        request_dose = input$var,
        group_var = !!map_chart_group,
        dose_type = !!map_chart_dose_vars
      )

      return(dat)
    })

    # Use the df_hc_bar inside the barplot function

    output$map_chart <- renderHighchart({
      hc_bar(
        hc_bar_dat = df_map_chart(),
        request_dose = input$var
      )
    })

    # MAP_TBL

    output$map_tbl <- reactable::renderReactable({
      dat <- df_map() %>%
        select(-c(iso_a3, lon, lat)) %>%
        mutate(across(
          any_of(grouping_levels),
          ~ if_else(total > 0, paste0(scales::number(.x), " (", round(.x / total * 100, digits = 1), "%)"), as.character(.x))
        )) %>%
        arrange(desc(total)) %>%
        relocate(total, .after = last_col()) %>% 
        reactable::reactable(
          highlight = TRUE,
          searchable = TRUE,
          compact = TRUE,
          defaultColDef = colDef(align = "right", format = colFormat(separators = TRUE, locales = "fr-Fr")),
          columns = list(
            country = colDef("Country", align = "left"),
            total = colDef("Total")
          )
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

      if (input$var == "Doses" & input$ts_date == "s_date_delivery") {
        df <- df_shipment %>%
          inner_join(
            df_data() %>% distinct(r_demand_id, r_mechanism, r_mechanism_type, r_status),
            by = c("s_r_demand_id" = "r_demand_id")
          )
      } else {
        df <- df_data()
      }
      # if (!is.null(country_select())) df %<>% filter(iso_a3 == country_select())

      req(input$ts_date %in% names(df), cancelOutput = TRUE)

      ts_var <- rlang::sym(input$var)
      ts_group <- rlang::sym(input$group)
      ts_date <- rlang::sym(input$ts_date)

      g_levels <- if (input$group == "r_mechanism_type") {
        grouping_levels[1:2]
      } else if (input$group == "r_mechanism") {
        grouping_levels[3:5]
      } else if (input$group == "r_status") {
        grouping_levels[6:9]
      }

      if (input$var == "Requests") {
        df_counts <- df %>%
          mutate(time_unit = as_date(floor_date(!!ts_date, unit = input$ts_unit))) %>%
          mutate(!!ts_group := factor(!!ts_group, levels = g_levels) %>% forcats::fct_na_value_to_level("Unknown")) %>%
          count(time_unit, !!ts_group) %>%
          arrange(time_unit)
      } else if (input$var == "Doses") {
        ts_dose <- rlang::sym(input$dose)

        df_counts <- df %>%
          mutate(time_unit = as_date(floor_date(!!ts_date, unit = input$ts_unit))) %>%
          count(time_unit, !!ts_group, wt = !!ts_dose) %>%
          mutate(!!ts_group := factor(!!ts_group, levels = g_levels) %>% forcats::fct_na_value_to_level("Unknown"))
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

      hc_pal <- set_pal(df_ts, input$group)

      hc %>%
        hc_title(text = NULL) %>%
        hc_chart(zoomType = "x") %>%
        hc_colors(hc_pal) %>%
        hc_xAxis(type = x_type, title = list(text = date_lab), crosshair = TRUE) %>%
        highcharter::hc_yAxis_multiples(
          list(
            title = list(text = isolate(var_lab())),
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

    delay_params <- reactive({
      dplyr::filter(delay_vars, var == input$delay_var)
    })

    df_delay <- reactive({
      delay_range <- delay_params()$range[[1]]
      date_1 <- rlang::sym(delay_range[1])
      date_2 <- rlang::sym(delay_range[2])

      df_delay <- app_data$df_delay %>%
        filter(r_mechanism_type == "Reactive", event %in% delay_range) %>%
        # filter to requests in pre-filtered df_data
        semi_join(df_data(), by = "r_demand_id") %>%
        select(r_demand_id, r_mechanism, r_mechanism_type, r_status, event, date) %>%
        pivot_wider(names_from = "event", values_from = "date") %>%
        mutate(delay = as.numeric({{ date_2 }} - {{ date_1 }}))
    })

    output$delay_boxplot <- renderHighchart({
      validate(need(nrow(df_delay()) > 0, "No Data to display"))

      delay_range <- delay_params()$range[[1]]
      date_1 <- rlang::sym(delay_range[1])
      expected_days <- delay_params()$expected_days
      delay_unit <- input$delay_unit

      df_boxplot <- df_delay() %>%
        drop_na(!!date_1) %>%
        mutate(time_unit = as_date(floor_date(!!date_1, unit = delay_unit)))
      
      complete_by <- if_else(
        delay_unit == "quarter",
        "3 months",
        input$delay_unit
      )

      frmt_time_unit <- switch(
        delay_unit,
        "year" = function(x) format(x, "%Y") %>% factor(),
        "quarter" = function(x) quarter(x, with_year = TRUE) %>% str_replace("\\.", "-Q") %>% factor(),
        "month" = function(x) format(x, "%Y-%m") %>% factor(),
        "week" = function(x) format(x, "%Y-W%V") %>% factor()
      )

      tu_range <- range(df_boxplot$time_unit)
      complete_tu <- seq.Date(tu_range[1], tu_range[2], by = complete_by)

      df_boxplot %<>%
        arrange(time_unit) %>%
        complete(time_unit = complete_tu) %>% # , fill = list(delay = NA)
        mutate(time_unit = frmt_time_unit(time_unit))
      
      hc_boxplot <- data_to_boxplot(
        df_boxplot,
        delay,
        time_unit,
        name = "Delay (days)",
        showInLegend = FALSE
      )

      highchart() %>%
        hc_chart(zoomType = "x") %>%
        hc_xAxis(
          type = "category",
          crosshair = TRUE,
          title = list(text = stringr::str_to_title(input$delay_unit))
        ) %>%
        hc_yAxis(
          title = list(text = "Delay (days)"),
          plotLines = list(
            list(
              color = "red", zIndex = 10, value = expected_days,
              label = list(text = paste("Expected", expected_days, "days"), verticalAlign = "bottom", textAlign = "left")
            )
          )
        ) %>%
        hc_add_series_list(hc_boxplot) %>%
        hc_tooltip(shared = TRUE) %>% 
        hc_caption(text = "Calculated for reactive vaccination campaigns only") %>%
        my_hc_export()
    })

    output$delay_hist <- renderHighchart({
      validate(need(nrow(df_delay()) > 0, "No Data to display"))
      df_delay <- df_delay()
      expected_days <- delay_params()$expected_days
      n_missing <- sum(is.na(df_delay$delay))

      df_hc <- df_delay %>%
        drop_na(delay) %>%
        count(delay)

      hchart(df_hc, "column", hcaes(delay, n), name = "Days") %>%
        hc_chart(zoomType = "x") %>%
        hc_title(text = NULL) %>%
        hc_xAxis(
          title = list(text = "Days"),
          allowDecimals = FALSE,
          crosshair = TRUE,
          min = 0,
          plotLines = list(
            list(
              color = "red", zIndex = 10, value = expected_days,
              label = list(text = paste("Expected", expected_days, "days"), verticalAlign = "top", textAlign = "left")
            )
          )
        ) %>%
        hc_yAxis(title = list(text = "Number of requests"), allowDecimals = FALSE) %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_tooltip(shared = TRUE) %>%
        hc_legend(
          title = list(text = ""),
          layout = "vertical",
          align = "right",
          verticalAlign = "top",
          x = -10,
          y = 40
        ) %>%
        hc_caption(text = "Calculated for reactive vaccination campaigns only") %>%
        hc_credits(enabled = TRUE, text = glue::glue("Unknown delay time for {scales::number(n_missing)} cases")) %>%
        my_hc_export()
    })

    output$delay_tbl <- gt::render_gt({
      validate(need(nrow(df_delay()) > 0, "No Data to display"))
      df <- df_delay()
      group <- input$group
      group_lab <- dplyr::if_else(group == "r_mechanism_type", "Mechanism", "Status")

      df %>%
        dplyr::select(.data[[group]], delay) %>%
        gtsummary::tbl_summary(
          by = input$group,
          label = list(delay ~ "Days between events"),
          type = list(delay ~ "continuous2"),
          digits = list(delay ~ c(0, 2, 2, 0, 0, 0, 0, 0)),
          statistic = gtsummary::all_continuous() ~ c(
            "{N_nonmiss}",
            "{mean} ({sd})",
            "{median} ({p25}, {p75})",
            "{min}, {max}"
          )
        ) %>%
        gtsummary::modify_header(update = gtsummary::all_stat_cols() ~ "**{level}**") %>%
        # gtsummary::add_overall(col_label = glue::glue("**{group_lab}**")) %>%
        gtsummary::italicize_levels() %>%
        gtsummary::modify_footnote(update = gtsummary::everything() ~ NA) %>%
        gtsummary::as_gt()
    })

    # ==========================================================================
    # TIMEVIS
    # ==========================================================================

    observe({
      df_demands <- df_data() %>%
        distinct(r_country, r_demand_id) %>%
        arrange(r_country, desc(r_demand_id))
      demands <- split(purrr::set_names(df_demands$r_demand_id), df_demands$r_country)
      shinyWidgets::updatePickerInput(session, "demand", choices = demands)
    })

    output$timevis <- renderTimevis({
      req(input$demand)

      df_out <- df_timevis %>%
        filter(r_demand_id == input$demand) %>%
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
