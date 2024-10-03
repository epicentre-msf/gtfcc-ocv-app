
mod_country_profile_ui <- function(id) {
  ns <- NS(id)

  countries <- df_country_profile |> 
    distinct(country_code, country_name) |> 
    drop_na() |> 
    arrange(country_name) %>%
    dplyr::pull(country_code)

  init_country <- "CMR"

  start_date <- df_country_profile |> 
    filter(country_code == init_country) |> 
    pull(date_min) |> 
    min(na.rm = TRUE)

  end_date <- Sys.Date() 

  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      shinyWidgets::pickerInput(
        inputId = ns("country"),
        label = "Country",
        choices = countries,
        choicesOpt = list(
          content = purrr::map(countries, flag_country)
        ),
        options = picker_opts(actions = FALSE, search = TRUE, style = "btn-outline-success")
      ),
      shiny::sliderInput(
        inputId = ns("time_period"),
        label = "Time period",
        min = start_date,
        max = end_date,
        value = c(start_date, end_date),
        width = "100%",
        timeFormat = "%d/%m/%y"
      )
      # shinyWidgets::radioGroupButtons(
      #   ns("period_jump"),
      #   label = "Jump to last:",
      #   choices = c("Full period" = 0, "3 years" = 3*12, "1 year" = 12, "6 months" = 6),
      #   size = "sm",
      #   status = "outline-success"
      # )
      # shiny::helpText("Jump to last:"),
      # actionButton(ns("period_6m"), "6 months", class = "btn-sm"),
      # actionButton(ns("period_1y"), "1 year", class = "btn-sm"),
      # actionButton(ns("period_3y"), "3 year", class = "btn-sm")
    ),
    bslib::layout_column_wrap(
      width = 1 / 5,
      fill = FALSE,
      bslib::value_box(
        title = "Campaigns displayed",
        value = textOutput(ns("camp_displayed")),
        htmlOutput(ns("camp_displayed_info")),
        theme_color = "success",
        # showcase = health_icon("symbols/cholera", type = "outline", height = 80),
        showcase = bs_icon("droplet")
      ),
      bslib::value_box(
        title = "Reactive campaigns",
        value = textOutput(ns("react_camp")),
        htmlOutput(ns("react_camp_info")),
        theme_color = "success",
        # showcase = health_icon("symbols/cholera", type = "outline", height = 80),
        showcase = bs_icon("droplet")
      ),
      bslib::value_box(
        title = "Doses distributed",
        value = textOutput(ns("dose_admi")),
        htmlOutput(ns("dose_admi_info")),
        theme_color = "success",
        # showcase = health_icon("symbols/cholera", type = "outline", height = 80),
        showcase = bs_icon("prescription2")
      ),
      bslib::value_box(
        title = "Targeted areas",
        value = textOutput(ns("target_areas")),
        htmlOutput(ns("target_areas_info")),
        theme_color = "success",
        # showcase = health_icon("symbols/geo_location", type = "outline", height = 80)
        showcase = bs_icon("geo")
      ), 
      bslib::value_box(
        title = "Last campaign",
        value = textOutput(ns("last_camp")),
        textOutput(ns("last_camp_info")),
        theme_color = "success",
        showcase = bs_icon("clock-history")
      )
    ),
    
    bslib::layout_columns(
      col_widths = c(6, 6, 6, 6),
      row_heights = c(2, 3),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex justify-content-start align-items-center",
          "Campaign timeline"
        ),
        bslib::card_body(padding = 0, timevis::timevisOutput(ns("timevis"), height = 300))
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
        bslib::card_body(padding = 0, highcharter::highchartOutput(ns("chart"), height = 300))
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
            ),
            sliderInput(
              ns("circle_size_mult"),
              label = "Circle size multiplyer",
              min = 1,
              max = 10,
              value = 6,
              step = 1,
              width = 200
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

mod_country_profile_server <- function(id, prep_dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # OBSERVERS ================================

    # observe({
    #   countries <- prep_dat |> 
    #     distinct(ref_adm0_name) |> 
    #     drop_na() |> 
    #     left_join(
    #       countrycode::codelist %>% transmute(ref_adm0_name = iso3c, country = cow.name),
    #       by = "ref_adm0_name"
    #     ) %>%
    #     arrange(country) %>%
    #     dplyr::pull(ref_adm0_name)

    #   shinyWidgets::updatePickerInput(
    #     session = session,
    #     inputId = "country",
    #     choices = countries,
    #     choicesOpt = list(
    #       content = purrr::map(countries, flag_country)
    #     )
    #   )
    # })

    observe({
      dmin <- prep_dat |> 
        filter(country_code == input$country) |> 
        pull(date_min) |> 
        min(na.rm = TRUE)
      # min_select <- dplyr::if_else(
      #   input$period_jump == 0,
      #   dmin,
      #   Sys.Date() - lubridate::period(as.numeric(input$period_jump), "months")
      # )
      # if (min_select < dmin) min_select <- dmin
      updateSliderInput(
        session,
        "time_period",
        min = dmin,
        max = Sys.Date(),
        value = c(dmin, Sys.Date())
      )
    }) |> bindEvent(input$country, ignoreInit = TRUE)
    
    # observeEvent(input$period_jump, ignoreInit = TRUE, {
    #   date_range <- c(Sys.Date() - lubridate::period(as.numeric(input$period_jump), "months"), Sys.Date())
    #   updateSliderInput(
    #     session,
    #     "time_period",
    #     value = date_range,
    #     timeFormat = "%d/%m/%y"
    #   )
    # })
    
    # observeEvent(input$period_6m, {
    #   date_range <- c(Sys.Date() - lubridate::period(6, "months"), Sys.Date())
    #   updateSliderInput(
    #     session,
    #     "time_period",
    #     value = date_range
    #   )
    # })
    
    # observeEvent(input$period_1y, {
    #   date_range <- c(Sys.Date() - lubridate::period(1, "year"), Sys.Date())
    #   updateSliderInput(
    #     session,
    #     "time_period",
    #     value = date_range
    #   )
    # })
    
    # observeEvent(input$period_3y, {
    #   date_range <- c(Sys.Date() - lubridate::period(3, "years"), Sys.Date())
    #   updateSliderInput(
    #     session,
    #     "time_period",
    #     value = date_range
    #   )
    # })
    
    observeEvent(country_df(), {
      shinyWidgets::updatePickerInput(
        session,
        "campaign",
        choices = request_summ()$request_id
      )
    })
    

    
    # filter the data for the selected country and date range
    country_df <- reactive({
      req(input$country)
      date_range <- as.Date(input$time_period)
      prep_dat %>%
        filter(
          country_code == input$country,
          between(date_start_d1, date_range[1], date_range[2]) |
            between(date_start_d2, date_range[1], date_range[2])
        )
    }) |> bindEvent(input$time_period)
    
    #filter the admin dict for country 
    admin_country <- reactive({ 
      req(input$country)
      admin_dict %>% 
        filter(adm0_iso3 == input$country) %>%  
        select(c(adm1_level, adm2_level, adm3_level)) 
    })
    
    admin_label <- reactive({unlist(admin_country()[1,])})
    
    # summarise the campaigns (using request id) for the country df
    request_summ <- reactive({
      req(nrow(country_df()) > 0)
      get_request_summ(country_df())
    })
    
    # get the latest campaign
    latest_camp <- reactive({
      req(nrow(country_df()) > 0)
      request_summ() %>%
        # rowwise() |> 
        # mutate(date_latest = max(c_across(contains("date_start")), na.rm = TRUE)) |> 
        # ungroup() |> 
        filter(date_max == max(date_max, na.rm = TRUE)) |> 
        slice_head(n = 1)
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
    
    # VALUE BOXES ==============================
    
    rounds_summ <- reactive({ get_rounds_summ(request_summ()) })
    
    output$camp_displayed <- renderText({
      glue::glue("{nrow(request_summ())}")
    })
    
    output$camp_displayed_info <- renderText({

      n_req_country <- reactive({ nrow(app_data$request %>% filter(iso_a3 == input$country, r_status == "Approved")) })
      
      percent_lab <- scales::percent( nrow(request_summ()) /n_req_country(), accuracy = 1)
      
      glue::glue("out of {n_req_country()} ({percent_lab}) due to missing data")
      
    })
    
    output$react_camp <- renderText({
      glue::glue( "{fmt_count(request_summ(), campaign_type == 'Reactive')}" )
    })
    
    output$react_camp_info <- renderText({
      glue::glue( "{fmt_count(request_summ(), n_rounds == 'single dose')} single dose campaigns" )
    })
    
    output$dose_admi <- renderText({
      glue::glue("{fmt_n_dose(sum(request_summ()$total_doses))}")
    })
    
    output$dose_admi_info <- renderText({
      
      n <- sum(request_summ()$n_d2)
      percent_dose <- n/sum(request_summ()$total_doses)
      percent_lab <- scales::percent(percent_dose, accuracy = 1)
      
      glue::glue("{fmt_n_dose(n)} ({percent_lab}) second doses")
    })
    
    output$target_areas <- renderText({
      admin_label <- admin_label()[[2]]
      glue::glue("{unique_admin()$unique_adm2} {admin_label}s")
    })
    
    output$target_areas_info <- renderText({
      admin_label <- admin_label()[[1]]
      glue::glue("in {unique_admin()$unique_adm1} {admin_label}s")
    })
    
    output$last_camp <- renderText({

      req(latest_camp())
      # browser()
      
      # date_var <- if( latest_camp()$n_rounds == "two doses" ) { sym("date_end_d2") } else { sym("date_end_d1") }
      
      date_end <- latest_camp() %>% pull(date_max)
      
      length <- time_length(lubridate::interval(date_end, Sys.Date()), "days")
      
      length_cat  <- case_when(
        length < 180 ~ "0-6 months ago",
        length >= 180 & length < 365 ~ "6m-1 year ago",
        length >= 365 & length < 1080 ~ "1-3 year ago",
        length >= 1080 ~ "more than 3 years ago" ) 
      
      glue::glue("{length_cat}")
      
    })
    
    output$last_camp_info <- renderText({
      
      # n_var <- if( latest_camp()$n_rounds == "two doses" ) { sym("n_d2") } else { sym("n_d1") }
      # n_label <- if(n_var == "n_d2") { "second"} else { "first" }
      
      glue::glue("{ fmt_n_dose( latest_camp()$n_d1 ) } 1st doses. { fmt_n_dose( latest_camp()$n_d2 ) } 2nd doses.")
      
    })
    
    # TIMEVIS/BARCHART ==========================
    
    output$timevis <- timevis::renderTimevis({
      req(nrow(country_df()) > 0)
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
        height = 300
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
      req(country_df())
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
      req(map_df())
      req(geo_select())

      geo_join <- geo_select()$join_by
      geo_col <- unname(geo_join)
      geo_col_sym <- rlang::sym(geo_col)
      geo_name_col <- geo_select()$name_var
      geo_name_col_sym <- rlang::sym(geo_name_col)
      geo_layer_name <- geo_select()$layer_name
      
      # filter sf polygons to only those found in dataset
      sf <- geo_select()$sf
      sf <- inner_join(sf, map_df(), by = c("adm0_iso3", geo_join))
      
      # save as reactive values
      rv$geo_join <- geo_join
      rv$geo_col <- geo_col
      rv$geo_col_sym <- geo_col_sym
      rv$geo_name_col <- geo_name_col
      rv$geo_name_col_sym <- geo_name_col_sym
      rv$geo_layer_name <- geo_layer_name
      rv$sf <- sf
    })
    
    # MAP
    output$map <- leaflet::renderLeaflet({
      
      bbox <- sf::st_bbox(filter(sf_world, iso_a3 == "CMR"))
      
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
        leaflet::removeControl("choro_leg")
      
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
            group = "Choropleth",
            layerId = "choro_leg"
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
            group = "Choropleth",
            layerId = "choro_leg"
          )
      }
      
      bbox <- sf::st_bbox(boundaries)
      leaflet::leafletProxy("map", session) %>%
        leaflet::flyToBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    })
    
    minicharts_init <- reactiveVal(TRUE)
    
    observe({
      
      if (isTruthy("Doses" %in% isolate(input$map_groups)) | minicharts_init())  {
        leaflet::leafletProxy("map", session) %>%
          leaflet.minicharts::clearMinicharts()
        
        # req(nrow(isolate(country_df())) > 0)
        boundaries <- rv$sf
        req(nrow(isolate(boundaries)) > 0)
        n_dose <- paste0(input$dose_var, "_dose_adm")
        # circle width
        pie_width <- (input$circle_size_mult * 10) * sqrt(boundaries[[n_dose]]) / sqrt(max(boundaries[[n_dose]]))
        
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
    })  %>% bindEvent(rv$sf, input$dose_var, rv$geo_name_col, input$circle_size_mult)
    
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
        pie_width <- (input$circle_size_mult * 10) * sqrt(boundaries[[n_dose]]) / sqrt(max(boundaries[[n_dose]]))
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
        pagination = FALSE
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
      country_code = ref_adm0_name,
      country_name = t_r_country,
      adm1_name = ref_adm1_name,
      adm2_name = ref_adm2_name,
      adm3_name = ref_adm3_name,
      adm4_name = ref_adm4_name,
      adm1_pcode = ref_adm1_pcode,
      adm2_pcode = ref_adm2_pcode,
      adm3_pcode = ref_adm3_pcode,
      adm4_pcode = ref_adm4_pcode,
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
    mutate(
      n_rounds = if_else(
        if_any(c(date_start_d1, date_start_d2), ~ is.na(.x)),
        "one dose",
        "two doses"
      ),
      date_end_d1 = coalesce(date_end_d1, date_start_d1),
      date_end_d2 = coalesce(date_end_d2, date_start_d2)
    ) %>%
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
      across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      d1_cov_adm = d1_dose_adm / target_pop,
      d2_cov_adm = d2_dose_adm / target_pop
    ) |> 
    rowwise() |> 
    mutate(
      date_min = min(c_across(contains("date_")), na.rm = TRUE),
      date_max = max(c_across(contains("date_")), na.rm = TRUE)
    ) |> 
    ungroup()
}

# function that summarize the requests by a country
get_request_summ <- function(country_df) {
  req_summ <- country_df %>%
    group_by(country_name, request_id) %>%
    summarise(
      date_start_d1 = min(date_start_d1, na.rm = TRUE),
      date_end_d1 = max(date_end_d1, na.rm = TRUE),
      date_start_d2 = min(date_start_d2, na.rm = TRUE),
      date_end_d2 = max(date_end_d2, na.rm = TRUE),
      date_min = max(date_min, na.rm = TRUE),
      date_max = max(date_max, na.rm = TRUE),
      n_rounds = unique(n_rounds),
      n_target_area = n(),
      n_adm1 = n_distinct(adm1_name, na.rm = TRUE),
      n_adm2 = n_distinct(adm2_name, na.rm = TRUE),
      n_adm3 = n_distinct(adm3_name, na.rm = TRUE),
      smallest_target = case_when(
        n_adm3 > 0 ~ "adm3",
        n_adm2 > 0 ~ "adm2",
        n_adm1 > 0 ~ "adm1",
        .default = "unknown"
      ),
      campaign_type = unique(campaign_type),
      target_type = paste0(unique(target_type), collapse = ", "),
      target_pop = sum(target_pop, na.rm = TRUE),
      n_d1 = sum(d1_dose_adm, na.rm = TRUE),
      cov_d1 = round(digits = 1, n_d1 / target_pop * 100),
      n_d2 = sum(d2_dose_adm, na.rm = TRUE),
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

  # browser()
  
  n_rounds <- unique(country_df$n_rounds)
  admin_sym <- sym(paste0(admin_level, "_pcode"))
  
  if (nrow(country_df) < 1) {
    return(dplyr::tibble(adm0_iso3 = character(), !!admin_sym := character()))
  }
  
  df <- country_df %>% group_by(!!admin_sym)

  if (filter_var == "largest_pop") {
    df <- df %>% filter(target_pop == max(target_pop))
  } else {
    df <- df %>% 
      mutate(date_end = coalesce(date_end_d2, date_end_d1, date_start_d2, date_start_d1)) |> 
      filter(date_end == max(date_end))
  }
  
  # if (filter_var == "largest_pop") {
  #   df <- df %>% filter(target_pop == max(target_pop))
  # } else if (filter_var == "latest_date" & isTruthy(n_rounds == "one dose")) {
  #   df <- df %>% filter(date_end_d1 == max(date_end_d1))
  # } else {
  #   df <- df %>% filter(date_end_d2 == max(date_end_d2))
  # }
  
  df %>%
    group_by(request_id, adm0_iso3 = country_code, !!admin_sym) %>% 
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

# get_map_data(
#   df_country_profile |> filter(country_code == "COD"),
#   "latest_date",
#   "adm1"
# )

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
