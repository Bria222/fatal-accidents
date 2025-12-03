library(tidyverse)
df <- read_csv("data/data_fatal/fatal_accidents_drivers.csv", show_col_types = FALSE)

cons_col <- intersect(c("constructor","team","constructor_name"), names(df))[1]
if (!is.na(cons_col)) {
  df %>% filter(!is.na(.data[[cons_col]])) %>%
    group_by(.data[[cons_col]]) %>% summarise(n = n()) %>%
    arrange(desc(n)) %>%
    head(10) %>% ggplot(aes(x = reorder(.data[[cons_col]], n), y = n)) +
    geom_col() + coord_flip() + labs(x = "Constructor", y = "Fatal incidents", title = "Top Constructors With Fatal Incidents")
  ggsave("docs/img/top_constructors_fatal.png", width = 8, height = 5)
} else {
  message("Constructor column not found. Please rename accordingly.")
}
