# data ingest
library(ffscrapr)
library(curl)
library(readr)

# data munging
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)

# get connection to Sleeper leagues
source("R/get_conn.R")

# get all franchise names and ids for all leagues
df_franchises <- purrr::map(lts_conn, ~ ffscrapr::ff_franchises(.x)) %>% 
  dplyr::bind_rows(.id = "league")

# current week
this_week <- difftime(lubridate::now(), lubridate::ymd("2023-09-06"), units = "weeks") %>% ceiling() %>% as.integer()

# get rosters for each week and team
df_rosters <- purrr::map(1:this_week, ~ purrr::map(lts_conn, ~ ffscrapr::ff_rosters(.x, .y), .y=.x)) %>% 
  purrr::map(~ dplyr::bind_rows(.x, .id = "league")) %>% 
  purrr::map(~ dplyr::bind_rows(.x, .id = "week")) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(week = as.integer(week)) %>% 
  # # get just the columns we need
  # dplyr::select()

# get scores for each league, week, and team
df_scoring_history <- purrr::map(lts_conn, ~ ffscrapr::ff_scoringhistory(.x, season = 2023)) %>% 
  dplyr::bind_rows(.id = "league") %>% 
  dplyr::select(league, week, sleeper_id, player_name, points)

# join scores to rosters
df_scored_rosters <- df_rosters %>% 
  dplyr::right_join(df_scoring_history, by = dplyr::join_by(week, league, player_id == sleeper_id))
