box::use(
  bslib,
  echarts4r,
  purrr[discard, map_chr, map, list_rbind],
  scales[dollar, percent],
  shiny,
  shinyjs[disable, disabled, enable],
  utils[tail]
)

box::use(
  app/logic/utils[player_is_addable, prettify_cost]
)

#' @export
ui <- function(id, player, teams_page = FALSE) {
  ns <- shiny$NS(id)
  
  card <-   bslib$card(
    height = 300,
    width = 100,
    bslib$card_body(
      bslib$navset_underline(
        bslib$nav_panel(
          "Summary",
          player_summary_ui(player)
        ),
        bslib$nav_panel(
          "Points",
          player_points_ui(ns, player)
        ),
        bslib$nav_panel(
          "Salary",
          player_salary_ui(ns, player)
        )
      )
    )
  )
  
  if (teams_page) {
    shiny$tagAppendChild(
      card,
      bslib$card_footer(
        shiny$tags$div(
          class = "btn-group d-flex",
          shiny$actionButton(ns("remove"), "Remove")
        )
      )
    )
  } else {
    shiny$tagAppendChild(
      card,
      bslib$card_footer(
        shiny$tags$div(
          class = "btn-group d-flex",
          shiny$actionButton(ns("add"), class = "btn-primary", "Add"),
          disabled(shiny$actionButton(ns("remove"), "Remove"))
        )
      )
    )
  }

}

player_summary_ui <- function(player) {
  shiny$tags$div(
    class = "d-flex flex-column gap-3 mt-2",
    shiny$tags$div(
      class = "d-flex flex-row flex-wrap gap-2",
      shiny$tags$div(
        class = "d-flex flex-column",
        shiny$tags$p(shiny$tags$strong("Name: "), player$known_name),
        shiny$tags$p(shiny$tags$strong("Team: "), player$squad$name),
        shiny$tags$p(shiny$tags$strong("Salary: "), prettify_cost(player$cost))
      ),
      shiny$tags$div(
        class = "d-flex flex-column",
        shiny$tags$p(shiny$tags$strong("Average Points: "), player$stats$avg_points),
        shiny$tags$p(shiny$tags$strong("Season Rank: "), player$stats$season_rank),
        shiny$tags$p(shiny$tags$strong("Owned by: "), percent(player$stats$owned_by))
      )
    )
  )
}

player_points_ui <- function(ns, player) {
  shiny$tags$div(
    class = "d-flex flex-column gap-3 mt-2",
    shiny$tags$div(
      shiny$tags$strong("Total Points: "),
      shiny$tags$span(player$stats$total_points)
    ),
    shiny$tags$div(
      shiny$tags$strong("Last Game Points: "),
      shiny$tags$span(player$stats$last_match_points)
    ),
    shiny$tags$div(
      shiny$tags$strong("Average Points: "),
      shiny$tags$span(player$stats$avg_points)
    ),
    shiny$tags$div(
      shiny$tags$strong("Season Points"),
      echarts4r$echarts4rOutput(ns("season_points"))
    )
  )
}

player_salary_ui <- function(ns, player) {
  shiny$tags$div(
    class = "d-flex flex-column gap-3 mt-2",
    shiny$tags$div(
      shiny$tags$strong("Current Salary: "),
      shiny$tags$span(
        prettify_cost(as.numeric(tail(player$stats$prices, 1)))
      )
    ),
    shiny$tags$div(
      shiny$tags$strong("Average Salary: "),
      prettify_cost(mean(as.numeric(player$stats$prices)))
    ),
    shiny$tags$div(
      shiny$tags$strong("Season Salary"),
      echarts4r$echarts4rOutput(ns("season_salary"))
    )
  )
}

#' @export
server <- function(id, player, team) {
  shiny$moduleServer(id, function(input, output, session) {
    
    output$season_points <- echarts4r$renderEcharts4r({
      
      all_rounds <- paste("Round", 1:4)
      
      player_rounds <- paste("Round", names(player$stats$scores))
      player_scores <- as.numeric(player$stats$scores)
      names(player_scores) <- player_rounds
      
      points <- map(
        all_rounds,
        \(round) {
          if (round %in% player_rounds) {
            data.frame(round = round, points = unname(player_scores[round]))
          } else {
            data.frame(round = round, points = NA)
          }
        }
      ) |> 
        list_rbind()
      
      points |> 
        echarts4r$e_charts(round) |> 
        echarts4r$e_line(points) |> 
        echarts4r$e_tooltip() |> 
        echarts4r$e_legend(show = FALSE) |> 
        echarts4r$e_color("#78c2ad")
      
    })
    
    output$season_salary <- echarts4r$renderEcharts4r({
      
      rounds <- paste("Round", names(player$stats$prices))
      player_salaries <- as.numeric(player$stats$prices)
      player_salaries <- player_salaries / 1e6
      salaries <- data.frame(round = rounds, salary = player_salaries)
      
      salaries |> 
        echarts4r$e_charts(round) |> 
        echarts4r$e_line(salary) |> 
        echarts4r$e_tooltip() |> 
        echarts4r$e_legend(show = FALSE) |> 
        echarts4r$e_color("#78c2ad") |> 
        echarts4r$e_format_y_axis(prefix = "$", suffix = "M")
      
    })
    
    shiny$observeEvent(input$add, {
      
      if (player_is_addable(player, team())) {
        disable("add")
        enable("remove")
        team(
          append(team(), list(player))
        )
      }
      
    })
    
    shiny$observeEvent(input$remove, {
      if (is.null(team())) {
        return()
      } else {
        enable("add")
        disable("remove")
        team(
          discard(team(), \(plr) plr$player_id == player$player_id)
        )
      }
    })
    
  })
}
