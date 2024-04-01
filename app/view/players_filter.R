box::use(
  config,
  purrr[map_chr, keep],
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
server <- function(id, all_players) {
  shiny$moduleServer(id, function(input, output, session) {
    
    players_filtered <- shiny$reactive({
      
      filtered_players <- all_players
      
      if (input$position != "Any") {
        filtered_players <- keep(filtered_players, \(player) player$position == input$position)
      }
      
      if (length(input$squad) >= 1) {
        filtered_players <- keep(filtered_players, \(player) player$squad$name %in% input$squad)
      }
      
      filtered_players
    })
    
    players_filtered
    
  })
}
