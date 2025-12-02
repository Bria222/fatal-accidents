# F1 Fatal Accident Analysis — Project

## How to run

### Website (GitHub Pages)

- Push the `docs/` folder to the root of the repository (or enable GitHub Pages from `docs/` branch).
- The static website will show `index.html`.

### Shiny

- Place your CSVs in `data/data_fatal/`:
  - fatal_accidents_drivers.csv
  - fatal_accidents_marshalls.csv
  - red_flags.csv
  - safety_cars.csv
- From RStudio, set working dir to `shiny/` and run `shiny::runApp("app.R")`.
- Or deploy to shinyapps.io and embed via iframe in `docs/index.html`.

## Notes

- The Shiny app contains heuristics to find common column names; if some plots are empty, inspect column names and update `find_col()` candidate lists in `shiny/app.R`.
