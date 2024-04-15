box::use(
  bslib,
  config,
  gargoyle[watch],
  purrr[map, map2, walk, map_chr],
  shiny
)

box::use(
  app/view/players_filter,
  app/view/players_sort,
  app/view/player_card
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tags$div(
    class = "p-4 m-4",
    shiny$tags$div(
      players_filter$ui(ns("filters")),
      players_sort$ui(ns("sort"))
    ),
    shiny$uiOutput(ns("player_cards")) 
  )
}


#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    players_filter$server("filters")
    players_sort$server("sort")
    
    output$player_cards <- shiny$renderUI({
      watch("filter_set")
      watch("sort_set")
      
      app <- session$userData$AppState
      team <- app$get_team()
      players <- app$get_sorted_and_filtered_players()
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
          on_team <- team$is_player_on_team(player)
          player_card$ui(id, player, on_team = on_team)
        }
      )

      map2(
        card_ids,
        players,
        \(id, player) {
          player_card$server(id, player)
        }
      )

      bslib$layout_column_wrap(
        width = 1/4,
        !!!player_cards
      )
      
    })
    
  })
}
