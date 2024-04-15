box::use(
  bslib,
  echarts4r,
  gargoyle[trigger],
  shiny,
  shinyjs[disable, disabled, enable],
  utils[tail]
)

box::use(
  app/logic/utils[prettify_cost]
)

#' @export
ui <- function(id, player, teams_page = FALSE, on_team = FALSE) {
  ns <- shiny$NS(id)
  
  card <-   bslib$card(
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
    
    if (on_team) {
      add_btn <- disabled(shiny$actionButton(ns("add"), class = "btn-primary", "Add"))
    } else {
      add_btn <- shiny$actionButton(ns("add"), class = "btn-primary", "Add")
    }
    
    if (on_team) {
      remove_btn <- shiny$actionButton(ns("remove"), "Remove")
    } else {
      remove_btn <- disabled(shiny$actionButton(ns("remove"), "Remove"))
    }
    
    shiny$tagAppendChild(
      card,
      bslib$card_footer(
        shiny$tags$div(
          class = "btn-group d-flex",
          add_btn,
          remove_btn
        )
      )
    )
  }

}

player_summary_ui <- function(player) {
  shiny$tags$div(
    class = "d-flex flex-column gap-3 mt-3 align-items-center",
    shiny$tags$h3(player$known_name, class="text-primary-emphasis"),
    shiny$tags$p(player$position, ", ", player$squad$name),
    shiny$tags$p(
      class = "fs-4",
      prettify_cost(player$cost)
    ),
    shiny$tags$div(
      class = "d-flex flex-row gap-2 justify-content-evenly",
      shiny$tags$p(
        shiny$tags$span(class = "text-primary-emphasis", "Average Points: "),
        player$stats$avg_points
      ),
      shiny$tags$p(
        shiny$tags$span(
          class="text-primary-emphasis", "Rank: "
        ),
        player$stats$season_rank
      )
    )
  )
}

player_points_ui <- function(ns, player) {
  shiny$tags$div(
    class = "d-flex flex-column gap-3 mt-2",
    shiny$tags$div(
      class = "d-flex flex-row gap-2 justify-content-evenly",
      shiny$tags$p(
        shiny$tags$span(class = "text-primary-emphasis" , "Total Points: "),
        player$stats$total_points
      ),
      shiny$tags$p(
        shiny$tags$span(class = "text-primary-emphasis", "Average Points: "),
        player$stats$avg_points
      )
    ),
    echarts4r$echarts4rOutput(ns("season_points"), height = 300)
  )
}

player_salary_ui <- function(ns, player) {
  shiny$tags$div(
    class = "d-flex flex-column gap-3 mt-2",
    shiny$tags$div(
      class = "d-flex flex-row gap-2 justify-content-evenly",
      shiny$tags$p(
        shiny$tags$span(class = "text-primary-emphasis", "Current Salary: "),
        prettify_cost(as.numeric(tail(player$stats$prices, 1)))
      ),
      shiny$tags$p(
        shiny$tags$span(class = "text-primary-emphasis", "Average Salary: "),
        prettify_cost(mean(as.numeric(player$stats$prices)))
      )
    ),
    echarts4r$echarts4rOutput(ns("season_salary"), height = 300)
  )
}

#' @export
server <- function(id, player) {
  shiny$moduleServer(id, function(input, output, session) {
    
    app <- session$userData$AppState
    team <- app$get_team()
    
    output$season_points <- echarts4r$renderEcharts4r({
      team$get_points_history(player) |>
        echarts4r$e_charts(round) |>
        echarts4r$e_line(points) |>
        echarts4r$e_tooltip() |>
        echarts4r$e_legend(show = FALSE) |>
        echarts4r$e_color("#78c2ad")
    })

    output$season_salary <- echarts4r$renderEcharts4r({
      team$get_salary_history(player) |>
        echarts4r$e_charts(round) |>
        echarts4r$e_line(salary) |>
        echarts4r$e_tooltip() |>
        echarts4r$e_legend(show = FALSE) |>
        echarts4r$e_color("#78c2ad") |>
        echarts4r$e_format_y_axis(prefix = "$", suffix = "M") |>
        echarts4r::e_grid(left = "15%")

    })
    
    session$userData$add_observers[[id]] <- shiny$observeEvent(input$add, {
      player_added <- team$add_player(player)
      if (player_added) {
        disable("add")
        enable("remove")
        trigger(paste0(player$position, "_updated"))
      }
    })
    
    session$userData$remove_observerse[[id]] <- shiny$observeEvent(input$remove, {
      team$remove_player(player)
      enable("add")
      disable("remove")
      trigger(paste0(player$position, "_updated"))
    })
    
  })
}
