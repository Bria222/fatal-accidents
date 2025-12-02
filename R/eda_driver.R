library(tidyverse)
df <- read_csv("data/data_fatal/fatal_accidents_drivers.csv", show_col_types = FALSE)

if ("year" %in% names(df)) {
  df %>% group_by(year) %>% summarise(fatal_count = n()) %>% ggplot(aes(x = year, y = fatal_count)) +
    geom_col() + labs(title = "Fatal Driver Incidents By Year")
} else {
  message("No 'year' column found. Please rename your year column.")
}


ggsave("docs/img/driver_fatal_by_year.png", width = 10, height = 4)
