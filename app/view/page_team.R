box::use(
  bslib,
  purrr[map, map_chr, map2, map_int, map_dbl, keep, pluck],
  shiny,
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
    shiny$uiOutput(ns("team_stats"), class = "d-flex flex-row gap-4 mb-2"),
    team_positions$ui(ns("forwards")),
    team_positions$ui(ns("mids")),
    team_positions$ui(ns("defs")),
    team_positions$ui(ns("gk"))
  )
}

#' @export
server <- function(id, team) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$team_stats <- shiny$renderUI({
      shiny$req(team())
      
      total_cost <- map_int(team(), "cost") |> 
        sum()
      
      total_avg_points <- map_dbl(team(), c("stats", "avg_points")) |> 
        sum()
      
      total_projected_points <- map_dbl(
        team(),
        pluck(list("stats", "projected_scores", 1))
      ) |> 
        sum()
      
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
    
    team_positions$server("forwards", "FWD", team)
    team_positions$server("mids", "MID", team)
    team_positions$server("defs", "DEF", team)
    team_positions$server("gk", "GK", team)
    
  })
}
