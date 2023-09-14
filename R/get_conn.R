lts_conn <- c(
  "991790283312459776",
  # "998064601466081280",
  "998064598509150208",
  "999100941309804544",
  "999101159296118784",
  "999101040358334464"
) %>%
  purrr::set_names(
    "L1 The Process",
    "L2 Sack Based Floor",
    "L3 The Only Legitimate LTS Bowl League",
    "L4 LTS League 4 (some)",
    "L5 Week 3 Nick Foles"
  ) %>% 
  # # 2022 IDs
  # lts_conn <- c(
  #     "869072831982559232",
  #     "859524057538887680",
  #     "869073624508891136",
  #     "869073848140800000",
  #     "866166165288939520"
  #   ) %>%
  # purrr::set_names("L1", "L2", "L3", "L4", "L5") %>%
  purrr::map(.x = .,
             ~ ffscrapr::ff_connect(
               league_id = .x,
               platform = "sleeper",
               # season = 2022
               season = 2023
             ))