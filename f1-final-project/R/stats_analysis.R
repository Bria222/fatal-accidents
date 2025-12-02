# stats_analysis.R
library(tidyverse)
library(broom)

df <- read_csv("data/data_fatal/fatal_accidents_drivers.csv", show_col_types = FALSE)

# Example hypothesis: Are driver ages different between fatal vs non-fatal races?
age_col <- intersect(c("age","driver_age"), names(df))[1]
status_col <- intersect(c("race_status","status"), names(df))[1]

if (!is.na(age_col) && !is.na(status_col)) {
  df2 <- df %>% filter(!is.na(.data[[age_col]]), !is.na(.data[[status_col]])) %>%
    mutate(status_bin = ifelse(tolower(.data[[status_col]]) %in% c("fatal","death","died","true","1"), "fatal", "null"))
  t_res <- t.test(.data[[age_col]] ~ status_bin, data = df2)
  broom::tidy(t_res) %>% write_csv("docs/stat_tests/t_test_age_status.csv")
  print(t_res)
} else {
  message("Missing age or status column: cannot perform t-test")
}

# Example linear regression: probability of fatal given age and wins (if wins available)
wins_col <- intersect(c("wins","win_count","total_wins"), names(df))
if (!is.na(age_col) && length(wins_col) > 0) {
  df3 <- df %>% mutate(is_fatal = ifelse(tolower(.data[[status_col]]) %in% c("fatal","death","died","true","1"), 1, 0))
  model <- glm(is_fatal ~ .data[[age_col]] + .data[[wins_col[1]]], data = df3, family = binomial)
  broom::tidy(model) %>% write_csv("docs/stat_tests/logistic_regression.csv")
  print(summary(model))
}
