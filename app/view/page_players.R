box::use(
  bslib,
  config,
  purrr[map, map2, walk, map_chr],
  shiny,
)

box::use(
  app/view/players_filter,
  app/view/players_sort,
  app/view/player_card
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$tags$div(
      players_filter$ui(ns("filters")),
      players_sort$ui(ns("sort"))
    ),
    shiny$uiOutput(ns("player_cards")) 
  )
}


#' @export
server <- function(id, all_players, team) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    players_filtered <- players_filter$server("filters", all_players)
    players_filtered_sorted <- players_sort$server("sort", players_filtered)
    
    output$player_cards <- shiny$renderUI({
      shiny$req(players_filtered_sorted())
      card_ids <- map_chr(players_filtered_sorted(), \(player) paste0("card_", player$player_id))
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
      
      team_player_ids <- map_chr(shiny$isolate(team()), \(player) player$player_id)
      
      player_cards <- map2(
        card_ids_ns,
        players_filtered_sorted(),
        \(id, player) {
          on_team <- 
            if (length(team_player_ids) > 0 && 
                player$player_id %in% team_player_ids)
              TRUE else FALSE
          player_card$ui(id, player, on_team = on_team)
        }
      )
      
      map2(
        card_ids,
        players_filtered_sorted(),
        \(id, player) {
          player_card$server(id, player, team)
        }
      )
      
      bslib$layout_column_wrap(
        width = 1/3,
        !!!player_cards
      )
      
    })
    
  })
}
