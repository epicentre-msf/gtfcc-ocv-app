
mod_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

mod_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
