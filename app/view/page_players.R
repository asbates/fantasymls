box::use(
  bslib,
  config,
  purrr[map, map2, map_dbl, map_chr, pluck, keep],
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
      
      player_cards <- map2(
        card_ids_ns,
        players_filtered_sorted(),
        \(id, player) {
          player_card$ui(id, player)
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
