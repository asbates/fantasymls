box::use(
  bslib,
  purrr[map, map_chr, map2, map_int, map_dbl, keep, pluck],
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
    shiny$uiOutput(ns("team_stats"), class = "d-flex flex-row gap-4"),
    shiny$uiOutput(ns("forwards"), class = "d-flex flex-row justify-content-center gap-2"),
    shiny$uiOutput(ns("midfielders"), class = "d-flex flex-row justify-content-center gap-2"),
    shiny$uiOutput(ns("defenders"), class = "d-flex flex-row justify-content-center gap-2"),
    shiny$uiOutput(ns("goalkeeper"), class = "d-flex flex-row justify-content-center gap-2")
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
          shiny$tags$h5("Cost"),
          shiny$tags$span(prettify_cost(total_cost))
        ),
        shiny$tags$div(
          class = "d-flex flex-column align-items-center",
          shiny$tags$h5("Average Points"),
          shiny$tags$span(total_avg_points)
        ),
        shiny$tags$div(
          class = "d-flex flex-column align-items-center",
          shiny$tags$h5("Projected Points"),
          shiny$tags$span(total_projected_points)
        )
      )
      
    })
    
    output$forwards <- shiny$renderUI({
      shiny$req(team())
      forwards <- keep(team(), \(player) player$position == "FWD")

      card_ids <- map_chr(forwards, \(player) paste0("card_", player$player_id))
      card_ids_ns <- map(card_ids, ns)

      player_cards <- map2(
        card_ids_ns,
        forwards,
        \(id, player) {
          player_card$ui(id, player, TRUE)
        }
      )

      map2(
        card_ids,
        forwards,
        \(id, player) {
          player_card$server(id, player, team)
        }
      )

      player_cards

    })
    
    output$midfielders <- shiny$renderUI({
      shiny$req(team())
      forwards <- keep(team(), \(player) player$position == "MID")
      
      card_ids <- map_chr(forwards, \(player) paste0("card_", player$player_id))
      card_ids_ns <- map(card_ids, ns)
      
      player_cards <- map2(
        card_ids_ns,
        forwards,
        \(id, player) {
          player_card$ui(id, player, TRUE)
        }
      )
      
      map2(
        card_ids,
        forwards,
        \(id, player) {
          player_card$server(id, player, team)
        }
      )
      
      player_cards
      
    })
    
    output$defenders <- shiny$renderUI({
      shiny$req(team())
      forwards <- keep(team(), \(player) player$position == "DEF")
      
      card_ids <- map_chr(forwards, \(player) paste0("card_", player$player_id))
      card_ids_ns <- map(card_ids, ns)
      
      player_cards <- map2(
        card_ids_ns,
        forwards,
        \(id, player) {
          player_card$ui(id, player, TRUE)
        }
      )
      
      map2(
        card_ids,
        forwards,
        \(id, player) {
          player_card$server(id, player, team)
        }
      )
      
      player_cards
      
    })
    
    output$goalkeeper <- shiny$renderUI({
      shiny$req(team())
      forwards <- keep(team(), \(player) player$position == "GK")
      
      card_ids <- map_chr(forwards, \(player) paste0("card_", player$player_id))
      card_ids_ns <- map(card_ids, ns)
      
      player_cards <- map2(
        card_ids_ns,
        forwards,
        \(id, player) {
          player_card$ui(id, player, TRUE)
        }
      )
      
      map2(
        card_ids,
        forwards,
        \(id, player) {
          player_card$server(id, player, team)
        }
      )
      
      player_cards
      
    })
    
  })
}
