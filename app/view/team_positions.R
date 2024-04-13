box::use(
  bslib,
  purrr[map, map_chr, map2, walk, keep],
  shiny,
)

box::use(
  app/view/player_card,
  app/logic/utils[prettify_cost]
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$uiOutput(
      ns("container"),
      class = "d-flex flex-row justify-content-center gap-3 mb-3"
    )
  )
}

#' @export
server <- function(id, position, team) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$container <- shiny$renderUI({
      shiny$req(team())
      players <- keep(team(), \(player) player$position == position)
      
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
          player_card$ui(id, player, TRUE)
        }
      )
      
      map2(
        card_ids,
        players,
        \(id, player) {
          player_card$server(id, player, team)
        }
      )
      
      player_cards
      
    })
    
  })
}
