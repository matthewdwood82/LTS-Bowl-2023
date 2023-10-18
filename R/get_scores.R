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

# data display and interaction
library(DT)

# get connection to Sleeper leagues
source("R/get_conn.R")

# get all franchise names and ids for all leagues
df_franchises <- purrr::map(lts_conn, ~ ffscrapr::ff_franchises(.x)) %>% 
  dplyr::bind_rows(.id = "league")

# current week
this_week <- difftime(lubridate::now(), lubridate::ymd("2023-09-06"), units = "weeks") %>% ceiling() %>% as.integer()

# week number for update
update_week <- difftime(
  lubridate::now(tz = "America/New_York"),
  lubridate::ymd_hms("2023-09-05 12:30:00", tz = "America/New_York"),
  units = "weeks"
) %>% 
  ceiling() %>% as.integer
readr::write_lines(this_week, "dat/update_week.txt")

# get all scores for each week
df_scores <- purrr::map(lts_conn, ~ ff_schedule(.x)) %>%
  dplyr::bind_rows(.id = "league") %>%
  # # will report interim week results
  # dplyr::filter(week <= this_week) %>% 
  # will only report the completed results week when the new week starts
  dplyr::filter(week <= this_week) %>%
  dplyr::mutate(diff_score = abs(franchise_score - opponent_score)) %>% 
  dplyr::left_join(df_franchises[,1:4], by = c("league", "franchise_id")) %>% 
  dplyr::left_join(df_franchises[,1:4], by = c("league" = "league", "opponent_id" = "franchise_id"), suffix = c("", "_opponent"))

# write df_scores
readr::write_csv(df_scores, "dat/df_scores.csv")

# define weekly categories so we can sort by ordered list in full df_weekly later
v_category <- ordered(c("Biggest Blowout", "Narrowest Win", "Fewest Points in Win", "Most Points in Loss", "Highest Score", "Lowest Score"))

df_weekly_blowout <- df_scores %>%
  dplyr::group_by(week) %>%
  dplyr::filter(result == "W", diff_score == max(diff_score)) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(Category = v_category[1]) %>% 
  dplyr::select(Week = week, Category, League = league, Awardee = franchise_name, Opponent = franchise_name_opponent, `Awardee Score` = franchise_score, `Opponent Score` = opponent_score, Difference = diff_score)

df_weekly_narrow_win <- df_scores %>%
  dplyr::group_by(week) %>%
  dplyr::filter(result == "W", diff_score == min(diff_score)) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(Category = v_category[2]) %>% 
  dplyr::select(Week = week, Category, League = league, Awardee = franchise_name, Opponent = franchise_name_opponent, `Awardee Score` = franchise_score, `Opponent Score` = opponent_score, Difference = diff_score)

df_weekly_fewest_points_win <- df_scores %>%
  dplyr::group_by(week) %>%
  dplyr::filter(result == "W") %>%
  dplyr::filter(franchise_score == min(franchise_score)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Category = v_category[3]) %>% 
  dplyr::select(Week = week, Category, League = league, Awardee = franchise_name, Opponent = franchise_name_opponent, `Awardee Score` = franchise_score, `Opponent Score` = opponent_score, Difference = diff_score)

df_weekly_most_points_loss <- df_scores %>%
  dplyr::group_by(week) %>%
  dplyr::filter(result == "L", opponent_score == max(opponent_score)) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(Category = v_category[4]) %>% 
  dplyr::select(Week = week, Category, League = league, Awardee = franchise_name, Opponent = franchise_name_opponent, `Awardee Score` = franchise_score, `Opponent Score` = opponent_score, Difference = diff_score)

df_weekly_highest_score <- df_scores %>%
  dplyr::group_by(week) %>%
  dplyr::filter(franchise_score == max(franchise_score)) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(Category = v_category[5]) %>% 
  dplyr::select(Week = week, Category, League = league, Awardee = franchise_name, Opponent = franchise_name_opponent, `Awardee Score` = franchise_score, `Opponent Score` = opponent_score, Difference = diff_score)

df_weekly_lowest_score <- df_scores %>%
  dplyr::group_by(week) %>%
  dplyr::filter(franchise_score == min(franchise_score)) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(Category = v_category[6]) %>% 
  dplyr::select(Week = week, Category, League = league, Awardee = franchise_name, Opponent = franchise_name_opponent, `Awardee Score` = franchise_score, `Opponent Score` = opponent_score, Difference = diff_score)


# collect weekly categories
v_weekly_dfs <- ls(pattern = "df_weekly_")

df_weekly <-mget(v_weekly_dfs) %>%
  purrr::set_names(nm = v_weekly_dfs) %>%
  dplyr::bind_rows() %>%
  dplyr::arrange(dplyr::desc(Week), factor(Category, levels = v_category))


# write df_weekly
readr::write_csv(df_weekly, "dat/df_weekly.csv")


# total points
df_total_points <- df_scores %>%
  dplyr::group_by(league, franchise_name) %>%
  dplyr::summarize(
    total_pts_for = sum(franchise_score, na.rm = TRUE),
    total_pts_against = sum(opponent_score, na.rm = TRUE)
  ) %>%
  # dplyr::ungroup() %>% 
  dplyr::select(
    Team = franchise_name,
    `Total Points For` = total_pts_for,
    `Total Points Against` = total_pts_against,
    League = league)


# write df_total_points
readr::write_csv(df_total_points, "dat/df_total_points.csv")


# survived teams
df_week_list <- df_scores %>% 
  dplyr::filter(week <= 16) %>% 
  dplyr::arrange(week) %>% 
  dplyr::group_by(league, franchise_id) %>% 
  dplyr::mutate(cum_franchise_score = cumsum(franchise_score)) %>% 
  dplyr::ungroup() %>% 
  # dplyr::group_by(week) %>% 
  # dplyr::arrange(franchise_score) %>% 
  # # tidyr::nest(.key = "week_df") %>%
  # dplyr::ungroup() %>%
  split(.$week) 

# get max week in data, i.e., the current week
v_max_week <- length(df_week_list)

# get survival table
df_survived <- df_week_list %>% 
  purrr::accumulate(\(x, d) {
    d %>% 
      dplyr::filter(franchise_name %in% x$franchise_name) %>%
      # to break ties, I add in a _very_ small portion of the cumulative franchise score
      # the effect is that any ties are broken using lowest cumulative score
      dplyr::slice_max(order_by = (franchise_score + .00001*cum_franchise_score), n = -3, with_ties = FALSE)}, .init = df_week_list$`1`) %>%  
  tail(-1)


# write df_survived
df_survived %>%
  dplyr::bind_rows() %>%
  dplyr::arrange(desc(week)) %>%
  dplyr::select(
    `Survival Week` = week,
    League = league,
    Team = franchise_name,
    Owner = user_name,
    Score = franchise_score,
    `Cumulative Score` = cum_franchise_score
  ) %>%
  readr::write_csv(., "dat/df_survived.csv")


# get eliminated table by anti-joining with survival table
df_eliminated <- purrr::map2(.x = df_week_list, .y = df_survived, ~dplyr::anti_join(.x, .y, by = c("league", "week", "franchise_id"))) %>% 
  dplyr::bind_rows() %>% 
  dplyr::group_by(league, franchise_id) %>% 
  dplyr::filter(week == min(week)) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(week)) %>% 
  dplyr::select(`Eliminated Week` = week, League = league, Team = franchise_name, Owner = user_name, Score = franchise_score, `Cumulative Score` = cum_franchise_score) 


# write df_eliminated
readr::write_csv(df_eliminated, "dat/df_eliminated.csv")
