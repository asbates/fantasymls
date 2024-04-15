box::use(
  bslib,
  gargoyle[watch],
  purrr[map, map_chr, map2, map_int, map_dbl, keep, pluck],
  shiny
)

box::use(
  app/view/player_card,
  app/view/team_positions,
  app/logic/utils[prettify_cost]
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tags$div(
    class = "p-4 m-4",
    shiny$uiOutput(ns("team_stats"), class = "d-flex flex-row gap-4 mb-4"),
    team_positions$ui(ns("forwards"), "FWD"),
    team_positions$ui(ns("mids"), "MID"),
    team_positions$ui(ns("defs"), "DEF"),
    team_positions$ui(ns("gk"), "GK")
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    app <- session$userData$AppState
    team <- app$get_team()
    
    output$team_stats <- shiny$renderUI({
      
      watch("FWD_updated")
      watch("MID_updated")
      watch("DEF_updated")
      watch("GK_updated")
      
      team_stats <- team$get_team_stats()
      total_cost <- team_stats$total_cost
      total_avg_points <- team_stats$total_avg_points
      total_projected_points <- team_stats$total_projected_points
      
      shiny$tagList(
        shiny$tags$div(
          class = "d-flex flex-column align-items-center",
          shiny$tags$h5("Cost", class = "text-primary-emphasis"),
          shiny$tags$span(prettify_cost(total_cost))
        ),
        shiny$tags$div(
          class = "d-flex flex-column align-items-center",
          shiny$tags$h5("Average Points", class = "text-primary-emphasis"),
          shiny$tags$span(total_avg_points)
        ),
        shiny$tags$div(
          class = "d-flex flex-column align-items-center",
          shiny$tags$h5("Projected Points", class = "text-primary-emphasis"),
          shiny$tags$span(total_projected_points)
        )
      )
      
    })
    
    team_positions$server("forwards", "FWD")
    team_positions$server("mids", "MID")
    team_positions$server("defs", "DEF")
    team_positions$server("gk", "GK")
    
  })
}
