box::use(
  gargoyle[trigger],
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
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    
    shiny$observeEvent(input$sort_by, {
      app <- session$userData$AppState
      app$set_sort(input$sort_by)
      trigger("sort_set")
    })
    
  })
}
