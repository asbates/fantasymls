box::use(
  purrr[keep, list_rbind, map, map_int, map_dbl, pluck]
)

#' @export
Team <- R6::R6Class(
  classname = "Team",
  private = list(
    players = list(),
    n_fwd = 0,
    n_mid = 0,
    n_def = 0,
    n_gk = 0,
    can_add = function(player) {
      if (player$player_id %in% names(private$players)) return(FALSE)
      if (player$position == "FWD" && private$n_fwd == 2) return(FALSE)
      if (player$position == "MID" && private$n_mid == 4) return(FALSE)
      if (player$position == "DEF" && private$n_def == 4) return(FALSE)
      if (player$position == "GK" && private$n_gk == 1) return(FALSE)
      
      return(TRUE)
    },
    increment_position_count = function(position) {
      if (position == "FWD") private$n_fwd <- private$n_fwd + 1
      if (position == "MID") private$n_mid <- private$n_mid + 1
      if (position == "DEf") private$n_def <- private$n_def + 1
      if (position == "GK")  private$n_gk <- private$n_gk + 1
    },
    decrement_position_count = function(position) {
      if (position == "FWD") private$n_fwd <- private$n_fwd - 1
      if (position == "MID") private$n_mid <- private$n_mid - 1
      if (position == "DEf") private$n_def <- private$n_def - 1
      if (position == "GK")  private$n_gk <- private$n_gk - 1
    }
  ),
  public = list(
    initialize = function() {},
    add_player = function(player) {
      if (private$can_add(player)) {
        private$players[[player$player_id]] <- player
        private$increment_position_count(player$position)
        return(TRUE)
      }
      return(FALSE)
    },
    remove_player = function(player) {
      private$players[[player$player_id]] <- NULL
      private$decrement_position_count(player$position)
    },
    get_forwards = function() {
      keep(
        private$players,
        \(player) player$position == "FWD"
      )
    },
    get_mids = function() {
      keep(
        private$players,
        \(player) player$position == "MID"
      )
    },
    get_defs = function() {
      keep(
        private$players,
        \(player) player$position == "DEF"
      )
    },
    get_gk = function() {
      keep(
        private$players,
        \(player) player$position == "GK"
      )
    },
    is_player_on_team = function(player) {
      player$player_id %in% names(private$players)
    },
    get_points_history = function(player) {
      all_rounds <- paste("Round", 1:8)

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
    },
    get_salary_history = function(player) {
      rounds <- paste("Round", names(player$stats$prices))
      player_salaries <- as.numeric(player$stats$prices)
      player_salaries <- player_salaries / 1e6
      data.frame(round = rounds, salary = player_salaries)
    },
    get_team_stats = function() {
      total_cost <- map_int(private$players, "cost") |> sum()
      total_avg_points <- map_dbl(private$players, c("stats", "avg_points")) |>
        sum()
      total_projected_points <- map_dbl(
        private$players,
        pluck(list("stats", "projected_scores", 1))
      ) |> 
        sum()
      
      list(
        total_cost = total_cost,
        total_avg_points = total_avg_points,
        total_projected_points = total_projected_points
      )
    }
  )
)
