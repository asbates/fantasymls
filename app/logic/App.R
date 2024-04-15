box::use(
  purrr[keep, map_chr, map_dbl, pluck]
)

box::use(
  app/logic/Team[Team]
)

#' @export
App <- R6::R6Class(
  classname= "App",
  private = list(
    all_players = NULL,
    squads = NULL,
    team = NULL,
    position_filter = "Any",
    squad_filter = c(),
    sort_by = "Average Points",
    filter_players = function() {
      position_filter <- private$position_filter
      squad_filter <- private$squad_filter
      filtered_players <- private$all_players
      
      if (position_filter != "Any") {
        filtered_players <- keep(
          filtered_players,
          \(player) player$position == position_filter
        )
      }
      
      if (length(squad_filter) >= 1) {
        filtered_players <- keep(
          filtered_players,
          \(player) player$squad$name %in% squad_filter
        )
      }
      
      filtered_players
    },
    sort_filtered_players = function() {
      filtered_players <- private$filter_players()
      sort_by <- private$sort_by
      decreasing <- TRUE
      
      if (sort_by == "Average Points") {
        sort_var <- map_dbl(
          filtered_players,
          \(player) {
            pluck(player, "stats", "avg_points")
          }
        )
      } else if (sort_by == "Price") {
        sort_var <- map_dbl(
          filtered_players,
          \(player) {
            pluck(player, "cost")
          }
        )
      } else if (sort_by == "Position") {
        sort_var <- map_chr(
          filtered_players,
          \(player) {
            pluck(player, "position")
          }
        )
        sort_var <- factor(sort_var, levels = c("FWD", "MID", "DEF", "GK"))
        decreasing <- FALSE
      } else if (sort_by == "Squad") {
        sort_var <- map_chr(
          filtered_players,
          \(player) {
            pluck(player, "squad", "name")
          }
        )
        decreasing <- FALSE
      }
      
      filtered_players[order(sort_var, decreasing = decreasing)]
      
    }
  ),
  public = list(
    initialize = function(all_players_file) {
      private$all_players <- readRDS(all_players_file)
      private$team <- Team$new()
    },
    get_all_players = function() {
      private$all_players
    },
    set_position_filter = function(value) {
      private$position_filter <- value
    },
    set_squad_filter = function(value) {
      private$squad_filter <- value
    },
    set_sort = function(by) {
      private$sort_by <- by
    },
    get_sorted_and_filtered_players = function() {
      private$sort_filtered_players()
    },
    get_team = function() {
      private$team
    }
  )
)
