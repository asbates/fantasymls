library(jsonlite)
library(purrr)

players_raw <- jsonlite::fromJSON("data/players.json", simplifyVector = FALSE)

vars_to_keep <- c(
  "first_name",
  "last_name",
  "known_name",
  "squad_id",
  "cost",
  "stats",
  "positions",
  "season_stats"
)

players <- map(players_raw, \(player) keep_at(player, vars_to_keep))

position_to_text <- function(pos) {
  switch(
    as.character(pos),
    "1" = "GK",
    "2" = "DEF",
    "3" = "MID",
    "4" = "FWD"
  )
}

players <- map(players, \(player) modify_in(player, "positions", \(pos) position_to_text(pos)))
players <- map(players, \(player) set_names(player, sub("positions", "position", names(player))))

squads <- jsonlite::fromJSON("data/squads.json", simplifyVector = FALSE)

squad_id_to_name <- function(squad_id) {
  squads |> 
    keep(\(squad) squad$id == squad_id) |> 
    map(\(squad) keep_at(squad, c("name", "short_name"))) |> 
    list_flatten()
}

players <- map(players, \(player) player <- c(player, list(squad = squad_id_to_name(player$squad_id))))
players <- map(players, \(player) discard_at(player, "squad_id"))


players <- imap(players, \(player, idx) player <- c(player, c(player_id = paste0("player", idx))))
players <- map(
  players,
  \(player) {
    if (is.null(player$known_name)) {
      new_name <- paste(player$first_name, player$last_name)
      modify_at(player, "known_name", \(x) new_name)
    } else {
      player
    }
  }
)

sounders <- keep(
  players,
  \(player) player$squad$name == "Seattle Sounders FC"
)
# PDLV is really a forward
sounders <- sounders |> 
  modify_if(
    \(player) player$known_name == "Pedro de la Vega",
    \(player) {
      player$position = "FWD"
      player
    }
  )

# subset some players so app isn't too slow
not_sounders <- keep(
  players,
  \(player) player$squad$name != "Seattle Sounders FC"
)
not_sounders_avg_points <- map_dbl(
  not_sounders,
  \(player) player$stats$avg_points
)
keep_idx <- which(not_sounders_avg_points > 4.5)
not_sounders <- not_sounders[keep_idx]

players <- append(sounders, not_sounders)

saveRDS(players, "app/data/players.rds")

squads <- squads |> 
  map(\(squad) keep_at(squad, c("name", "short_name")))

saveRDS(squads, "app/data/squads.rds")
