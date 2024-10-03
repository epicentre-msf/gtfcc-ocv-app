app_server <- function(input, output, session) {
  
  # error page
  sever(html = disconnected, bg_color = "white", color = "black")
  
  # user logs
  track_usage(
    storage_mode = store_json(path = here::here("logs")),
    app_name = app_name,
    what = c("session"),
    exclude_users = c("paul.campbell@epicentre.msf.org", "paul")
  )
  
  # app guide info
  # guide <- Cicerone$
  #   new()$
  #   step(
  #   "guide",
  #   "Guide",
  #   "Click this guide button at any time for a guided tour of the dashboard.",
  #   close_btn_text = "OK!",
  #   position = "bottom-center"
  # )
  # guide$init()$start()
  
  # server modules 
  mod_request_server("request")
  mod_timevis_server("timevis")
  
  country_tab_init <- TRUE
  observeEvent(input$tabs, {
    if (all(input$tabs == "country", country_tab_init)) {
      country_tab_init <<- FALSE
      mod_country_profile_server("country", df_country_profile)
    }
  })
  
}