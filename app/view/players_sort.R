box::use(
  purrr[map_chr, map_dbl, pluck],
  shiny
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tags$div(
    class = "d-flex flex-row align-items-center gap-4",
    shiny$tags$p("Sort by"),
    shiny$selectInput(
      ns("sort_by"),
      label = NULL,
      choices = c(
        "Average Points",
        "Price",
        "Position",
        "Squad"
      )
    )
  )
}

#' @export
server <- function(id, players_filtered) {
  shiny$moduleServer(id, function(input, output, session) {
    
    players_filtered_sorted <- shiny$reactive({
      
      decreasing <- TRUE
      
      if (input$sort_by == "Average Points") {
        sort_var <- map_dbl(
          players_filtered(),
          \(player) {
            pluck(player, "stats", "avg_points")
          }
        )
      } else if (input$sort_by == "Price") {
        sort_var <- map_dbl(
          players_filtered(),
          \(player) {
            pluck(player, "cost")
          }
        )
      } else if (input$sort_by == "Position") {
        sort_var <- map_chr(
          players_filtered(),
          \(player) {
            pluck(player, "position")
          }
        )
        sort_var <- factor(sort_var, levels = c("FWD", "MID", "DEF", "GK"))
        decreasing <- FALSE
      } else if (input$sort_by == "Squad") {
        sort_var <- map_chr(
          players_filtered(),
          \(player) {
            pluck(player, "squad", "name")
          }
        )
        decreasing <- FALSE
      }
      
      players_filtered()[order(sort_var, decreasing = decreasing)]
      
    })
    
    players_filtered_sorted
    
  })
}
