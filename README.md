# F1 Fatal Accident Analysis — Project

[![Shiny App](https://img.shields.io/badge/Shiny-App-blue)](https://fatal-accidents.shinyapps.io/shiny/)  
[![License](https://img.shields.io/badge/License-MIT-green)]()

---

## Project Overview

This project explores **historical fatal accidents in Formula 1**, analyzing drivers, marshals, red flags, and safety car deployments. It includes:

- Interactive **Shiny dashboard** for exploring accidents and driver statistics.
- **Static website** showcasing the analysis, key insights, and visualizations.
- **Data cleaning & preprocessing** steps to ensure consistent, analysis-ready datasets.

---

## Prerequisites

- R ≥ 4.2 with the following packages installed:
  ```R
  install.packages(c(
    "shiny", "leaflet", "dplyr", "ggplot2",
    "readr", "glue", "shinycssloaders",
    "lubridate", "stringr", "scales"
  ))
  ```
