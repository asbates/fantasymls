
box::use(
  purrr[map_chr, map_dbl, map_int, keep],
  scales[dollar]
)


#' @export
player_is_addable <- function(player, team) {
  
  can_add <- TRUE
  
  current_team_ids <- purrr::map_chr(team, "player_id")
  
  if (player$player_id %in% current_team_ids) {
    can_add <- FALSE
  }
  
  n_keepers <- keep(team, \(player) player$position == "GK") |> 
    length()
  n_def <- keep(team, \(player) player$position == "DEF") |> 
    length()
  n_mid <- keep(team, \(player) player$position == "MID") |> 
    length()
  n_fwd <- keep(team, \(player) player$position == "FWD") |> 
    length()
  
  if (player$position == "GK" && n_keepers == 1) {
    can_add <- FALSE
  } 
  if (player$position == "DEF" && n_def == 4) {
    can_add <- FALSE
  } 
  if (player$position == "MID" && n_mid == 4) {
    can_add <- FALSE
  }
  if (player$position == "FWD" && n_fwd == 2) {
    can_add <- FALSE
  }
  
  can_add
}

#' @export
add_player_to_team <- function(player, team) {
  
  if (length(team) == 0) {
    return(player)
  }
  
  if (!player_is_addable(player, team)) {
    return(team)
  }
  
  current_team_ids <- purrr::map_chr(team, "player_id")
  
  if (player$player_id %in% current_team_ids) {
    return(team)
  }
  
  # append(team, player)
  
  
}

#' @export
prettify_cost <- function(cost) {
  dollar(cost, accuracy = 0.1, scale = 0.000001, suffix = "M") 
}

