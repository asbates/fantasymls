box::use(
  bslib,
  config,
  gargoyle[init],
  shiny[moduleServer, NS, tags, tagList],
  shinyjs[useShinyjs]
)

box::use(
  app/view/page_players,
  app/view/page_team,
  app/logic/App[App]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    bslib$page_navbar(
      title = "Fantasy MLS Manager",
      theme = bslib$bs_theme(
        version = 5,
        bootswatch = "minty"
      ),
      bslib$nav_panel(
        title = "Players",
        page_players$ui(ns("players"))
      ),
      bslib$nav_panel(
        title = "My Team",
        page_team$ui(ns("team"))
      )
    ) 
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    session$userData[["AppState"]] <- App$new(config$get("players"))
    
    init("filter_set")
    init("sort_set")
    init("FWD_updated", "MID_updated", "DEF_updated", "GK_updated")
    
    page_players$server("players")
    page_team$server("team")
    
  })
}
