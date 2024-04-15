box::use(
  bslib,
  gargoyle[watch],
  purrr[map, map_chr, map2, walk],
  shiny
)

box::use(
  app/view/player_card,
  app/logic/utils[prettify_cost]
)

#' @export
ui <- function(id, position) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$uiOutput(
      ns("container"),
      class = paste("mb-4", position)
    )
  )
}

#' @export
server <- function(id, position, team) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    app <- session$userData$AppState
    team <- app$get_team()
    
    output$container <- shiny$renderUI({
      watch(paste0(position, "_updated"))
      
      players <- switch(
        position,
        "FWD" = team$get_forwards(),
        "MID" = team$get_mids(),
        "DEF" = team$get_defs(),
        "GK" = team$get_gk()
      )
      
      card_ids <- map_chr(players, \(player) paste0("card_", player$player_id))
      card_ids_ns <- map(card_ids, ns)
      
      walk(
        names(session$userData$add_observers),
        \(observer_id) {
          if (observer_id %in% card_ids) {
            session$userData$add_observers[[observer_id]]$destroy()
            session$userData$add_observers[[observer_id]] <- NULL
          }
        }
      )
      walk(
        names(session$userData$remove_observers),
        \(observer_id) {
          if (observer_id %in% card_ids) {
            session$userData$remove_observers[[observer_id]]$destroy()
            session$userData$remove_observers[[observer_id]] <- NULL
          }
        }
      )
      
      player_cards <- map2(
        card_ids_ns,
        players,
        \(id, player) {
          player_card$ui(id, player, teams_page = TRUE)
        }
      )
      
      map2(
        card_ids,
        players,
        \(id, player) {
          player_card$server(id, player)
        }
      )
      
      player_cards
      
    })
    
  })
}
