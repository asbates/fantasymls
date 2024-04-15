box::use(
  config,
  gargoyle[trigger],
  purrr[map_chr],
  shiny
)

squads <- readRDS(config$get("squads"))

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tags$div(
    class = "d-flex flex-row align-items-center gap-4",
    shiny$tags$p("Filter by"),
    shiny$selectInput(
      ns("position"),
      "Position",
      choices = c("Any", "FWD", "MID", "DEF", "GK")
    ),
    shiny$selectInput(
      ns("squad"),
      "Squad",
      choices = map_chr(squads, "name"),
      multiple = TRUE
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    
    shiny$observeEvent(input$position, {
      app <- session$userData$AppState
      app$set_position_filter(input$position)
      trigger("filter_set")
    })
    
    shiny$observeEvent(input$position, {
      app <- session$userData$AppState
      app$set_squad_filter(input$squad)
      trigger("filter_set")
    })
    
  })
}
