library(tidyverse)
library(nflreadr)
library(piggyback)

# variables ----
cup_weeks <- c(3,5,10,15)
current_season <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week(TRUE)
current_week <- 3

if (nflreadr::get_current_week(TRUE) %in% cup_weeks) {
  # load mfl chat messages ----
  messages <- xml2::read_xml(paste0("https://www45.myfantasyleague.com/fflnetdynamic", nflreadr::get_current_season(TRUE), "/54277_chat.xml")) %>%
    xml2::xml_contents() %>%
    xml2::xml_attrs() %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(".") %>%
    dplyr::filter(to == "0000" & grepl("subject:leagueCup-", message)) %>%
    dplyr::mutate(
      game_id = stringr::str_extract(message, "(?<=subject:leagueCup-).*?(?=-team)"), # get gameId zwischen leagueCup- und -team
      game_id = stringr::str_remove(game_id, "gameId:"), # remove gameId:
      team_id = stringr::str_extract(message, "(?<=-team).*?(?=-)"), # get teamId zwischen -team und -
      team_id = stringr::str_remove(team_id, ":"), # remove :
      round = as.integer(stringr::str_extract(game_id, "^[^-]+")), # first number in gameId to first "-"
      week = as.integer(cup_weeks[round]),  # get cup_weeks[n] where n equals the round
      player_id = stringr::str_extract(message, "\\[([^\\]]+)\\]"), # get player_id between []
      player_id = stringr::str_remove(player_id, "\\["), # remove [
      player_id = stringr::str_remove(player_id, "\\]"), # remove ]
    ) %>%
    dplyr::group_by(game_id, team_id) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(game_id, round, week, team_id, player_id) %>%
    tidyr::separate_rows(player_id, sep = ",") %>%
    dplyr::filter(week == current_week) %>%
    dplyr::mutate(
      position = stringr::str_split(player_id, "-", simplify = TRUE)[,1],
      player_id = stringr::str_split(player_id, "-", simplify = TRUE)[,2]
    )

  current_round <- messages %>%
    dplyr::select(round) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  # nflreadr data ----
  ## offense ----
  player_stats_offense <- nflreadr::load_player_stats(stat_type = "offense") %>%
    dplyr::filter(week == current_week) %>%
    dplyr::mutate(
      touchdowns = passing_tds + rushing_tds + receiving_tds + special_teams_tds,
      turnover = interceptions + sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost
    ) %>%
    dplyr::select(season, week, player_id, player_display_name, recent_team, passing_yards, rushing_yards, receiving_yards, touchdowns, turnover) %>%
    dplyr::rename(team = recent_team) %>%
    tidyr::gather(category, value, c(passing_yards:turnover))

  ## kicking ----
  player_stats_kicking <- nflreadr::load_player_stats(stat_type = "kicking") %>%
    dplyr::filter(week == current_week) %>%
    dplyr::select(
      season, week, player_id, player_display_name, team, fg_made_distance
    ) %>%
    tidyr::gather(category, value, fg_made_distance) %>%
    dplyr::mutate(category = "special_teams_yards")

  ## punting ----
  player_stats_punting <- nflreadr::load_pbp() %>%
    dplyr::filter(play_type == "punt" & punt_attempt == 1 & week == current_week) %>%
    dplyr::group_by(punter_player_id) %>%
    dplyr::summarise(
      season = dplyr::first(season),
      week = dplyr::first(week),
      punt_yards = sum(kick_distance),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      nflreadr::load_ff_playerids() %>%
        dplyr::select(gsis_id, team, name),
      by = c("punter_player_id" = "gsis_id")
    ) %>%
    dplyr::rename(player_id = punter_player_id, player_display_name = name) %>%
    dplyr::select(season, week, player_id, player_display_name, team, punt_yards) %>%
    tidyr::gather(category, value, punt_yards) %>%
    dplyr::mutate(category = "special_teams_yards")

  ## defense ----
  player_stats_defense <- nflreadr::load_player_stats(stat_type = "defense") %>%
    dplyr::filter(week == current_week) %>%
    dplyr::mutate(def_turnover = def_interceptions + def_fumbles_forced) %>%
    dplyr::select(season, week, player_id, player_display_name, team, def_tackles, def_tackles_for_loss, def_sacks, def_qb_hits, def_turnover) %>%
    tidyr::gather(category, value, c(def_tackles:def_turnover))

  ##  snap counts ----
  player_snap_counts <- nflreadr::load_snap_counts() %>%
    dplyr::filter(week == current_week) %>%
    dplyr::select(pfr_player_id, week, offense_snaps, defense_snaps, st_snaps)

  ## combine ----
  player_stats <- messages %>%
    dplyr::left_join(
      nflreadr::load_ff_playerids() %>%
        dplyr::select(gsis_id, mfl_id, pfr_id),
      by = c("player_id" = "mfl_id")
    ) %>%
    left_join(
      player_snap_counts,
      by = c("pfr_id" = "pfr_player_id", "week")
    ) %>%
    dplyr::left_join(
      rbind(player_stats_offense, player_stats_defense, player_stats_kicking, player_stats_punting),
      by = c("gsis_id" = "player_id", "week"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(!is.na(category)) %>%
    dplyr::group_by(round, team_id, category) %>%
    dplyr::mutate(
      player_count = n(),
      cat_sum = sum(value),
      season = nflreadr::get_current_season(),
      game_id = paste(season, game_id, sep = "-")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(game_id, season, everything()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      snaps = sum(offense_snaps, defense_snaps, st_snaps, na.rm = TRUE)
    )

  # create winner/loser data----
  category_sums <- player_stats %>%
    dplyr::select(game_id, team_id, category, cat_sum, player_count) %>%
    dplyr::distinct() %>%
    dplyr::group_by(game_id) %>%
    dplyr::arrange(team_id) %>%
    dplyr::mutate(
      opponent_id = ifelse(dplyr::last(team_id) == team_id, dplyr::first(team_id), dplyr::last(team_id))
    ) %>%
    dplyr::ungroup()

  category_wins <- category_sums %>%
    dplyr::left_join(
      category_sums %>%
        dplyr::rename_at(vars(cat_sum, player_count),function(x) paste0(x,"_opponent")) %>%
        dplyr::select(-game_id, -opponent_id),
      by = c("opponent_id" = "team_id", "category")
    ) %>%
    dplyr::mutate(
      result_sum = dplyr::case_when(
        category != "turnover" & cat_sum < cat_sum_opponent ~ 0,
        category == "turnover" & cat_sum > cat_sum_opponent ~ 0,
        cat_sum == cat_sum_opponent ~ 0.5,
        TRUE ~ 1
      ),
      result_count = dplyr::case_when(
        player_count > player_count_opponent ~ 0,
        player_count == player_count_opponent ~ 0.5,
        TRUE ~ 1
      ),
      cat_result = dplyr::case_when(
        result_sum == 1 | (result_sum == 0.5 & result_count == 1) ~ 1,
        result_sum == 0 | (result_sum == 0.5 & result_count == 0) ~ 0,
        TRUE ~ 0.5
      ),
      cat_winner = dplyr::case_when(
        cat_result == 1 ~ team_id,
        cat_result == 0 ~ opponent_id
      )
    ) %>%
    dplyr::group_by(game_id, team_id) %>%
    dplyr::mutate(
      franchise_score = sum(cat_result)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(game_id, team_id, category, cat_result, cat_winner, franchise_score)

  matchup_wins <- category_wins %>%
    dplyr::select(game_id, team_id, franchise_score) %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      player_stats %>%
        dplyr::group_by(game_id, team_id) %>%
        dplyr::summarise(
          snaps_sum = sum(snaps, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("game_id", "team_id")
    ) %>%
    dplyr::group_by(game_id) %>%
    dplyr::arrange(team_id) %>%
    dplyr::mutate(
      opponent_score = ifelse(dplyr::row_number() == 1, dplyr::lead(franchise_score), dplyr::lag(franchise_score)),
      matchup_result = dplyr::case_when(
       franchise_score < opponent_score ~ 0,
       franchise_score == opponent_score ~ 0.5,
       TRUE ~ 1
       ),
      winner = dplyr::case_when(
        matchup_result == 1 ~ team_id,
        matchup_result == 0.5 & snaps_sum < dplyr::lag(snaps_sum) ~ team_id,
        matchup_result == 0.5 & snaps_sum < dplyr::lead(snaps_sum) ~ team_id,
      )
    ) %>%
    dplyr::select(-dplyr::ends_with("_score")) %>%
    dplyr::arrange(winner) %>%
    tidyr::fill(winner) %>%
    dplyr::ungroup()

  # create output data ----
  output_data <- player_stats %>%
    dplyr::left_join(
      category_wins,
      by = c("game_id", "team_id", "category")
    ) %>%
    dplyr::left_join(
      matchup_wins,
      by = c("game_id", "team_id")
    ) %>%
    dplyr::filter(!is.na(category)) %>%
    dplyr::select(-gsis_id, -pfr_id, -offense_snaps, -defense_snaps, -st_snaps)

  if(current_round == 1) {
    df_to_json <- output_data
  } else {
    cli::cli_alert_info("Write Data")
    readr::write_csv(war, paste0("rfl_war_", current_season, ".csv"))

    old_data <- readr::read_csv(paste0("https://github.com/bohndesverband/cl-data/releases/download/league_cup/league-cup-", current_season, ".csv"))

    df_to_json <- rbind(
      old_data %>%
        dplyr::filter(week < current_week),
      output_data
    )
  }

  cli::cli_alert_info("Write csv Data")
  readr::write_csv(df_to_json, paste0("league-cup-", current_season, ".csv"))

  cli::cli_alert_info("Upload csv Data")
  piggyback::pb_upload(paste0("league-cup-", current_season, ".csv"), "bohndesverband/cl-data", "league_cup", overwrite = TRUE)

  # convert to json ----
  json_data <- df_to_json %>%
    dplyr::filter(value > 0) %>%
    tidyr::nest(
      players = c(player_id, player_display_name, position, team, value, snaps)
    ) %>%
    dplyr::group_by(game_id, team_id) %>%
    tidyr::nest(
      categories = c(category, cat_winner, player_count, cat_sum, cat_result, players)
    ) %>%
    tidyr::nest(
      franchises = c(team_id, franchise_score, matchup_result, snaps_sum, categories)
    ) %>%
    tidyr::nest(
      matches = c(game_id, winner, franchises)
    )

  cli::cli_alert_info("Write json Data")
  jsonlite::write_json(json_data, paste0("league-cup-", current_season, ".json"))

  cli::cli_alert_info("Upload json Data")
  piggyback::pb_upload(paste0("league-cup-", current_season, ".json"), "bohndesverband/cl-data", "league_cup", overwrite = TRUE)

  timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  write(timestamp, "timestamp.json")
  piggyback::pb_upload("timestamp.json", "bohndesverband/cl-data", "league_cup", overwrite = TRUE)
}
