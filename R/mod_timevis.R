
mod_timevis_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "reactive-width",
    tagList(
      shinyWidgets::radioGroupButtons(
        ns("period"),
        label = NULL,
        choices = c("Current year" = "ytd", "Past 6 months" = "6m", "Past 12 months" = "12m", "All period" = "all"),
        selected = "ytd",
        size = "sm"
      ),
      shinyWidgets::pickerInput(
        inputId = ns("country"),
        label = NULL,
        choices = "",
        options = picker_opts(search = TRUE, none_text = "All countries"),
        multiple = TRUE,
        width = 200
      ),
      shinyWidgets::pickerInput(
        inputId = ns("event"),
        label = NULL,
        choices = c("Requests" = "request", "Decisions" = "decision", "Shipments" = "shipment", "Rounds" = "round"),
        selected = NULL, # c("request", "decision", "shipment", "round"),
        options = picker_opts(none_text = "All events"),
        multiple = TRUE,
        width = 200
      )
    ) %>% map(div_inline),
    timevis::timevisOutput(ns("timevis"))
  )
}

mod_timevis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    date_range <- reactive({
      end <- today()
      start <- case_when(
        input$period == "ytd" ~ floor_date(end, "year"),
        input$period == "6m" ~ end - months(6),
        input$period == "12m" ~ end - months(12),
        input$period == "all" ~ min(df_timevis$start)
      )
      tibble::lst(start, end)
    })
    
    observe({
      countries <- df_timevis %>% filter(start >= date_range()$start) %>% pull(group) %>% unique() %>% sort()
      selected <- intersect(input$country, countries)
      shinyWidgets::updatePickerInput(session, "country", choices = countries, selected = selected)
    })
    
    tv_data <- reactive({
      df_tv <- df_timevis %>% filter(start >= date_range()$start)
      if (length(input$country)) df_tv %<>% filter(group %in% input$country)
      if (length(input$event)) df_tv %<>% filter(event %in% input$event)
      df_groups <- distinct(df_tv, id = group) %>% mutate(content = glue::glue("<b>{id}</b>")) %>% arrange(id)
      tibble::lst(df_tv, df_groups)
    })
    
    output$timevis <- renderTimevis({
      timevis(
        tv_data()$df_tv,
        groups = tv_data()$df_groups,
        options = list(
          start = isolate(date_range()$start - 5),
          end = isolate(date_range()$end + 14)
          # timeAxis = list(scale = "day")
        )
      )
    })
    
  })
}
