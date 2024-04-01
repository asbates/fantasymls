box::use(
  bslib,
  config,
  shiny[moduleServer, NS, tags, reactiveVal, tagList],
  shinyjs[useShinyjs],
  purrr[map]
)

box::use(
  app/view/page_players,
  app/view/page_team
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
    
    all_players <- readRDS(config$get("players"))
    team <- reactiveVal()
    Players$new()
    Team$new()
    
    # ---------- notes ------------
    # could we have page_players return the team object?
    # it may help solidify the point that it's just a middle man
    # and from it's viewpoint, team is just a bookkeeping task
    # which means it's a potential failure point from a simple mistake, that
    #  shouldn't have any affect
    # maybe don't even take it as an argument too
    page_players$server("players", all_players, team)
    page_team$server("team", team)
    
  })
}
