# shiny/app.R
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(glue)
library(shinycssloaders)

# ---- utility: attempt to read columns robustly ----
safe_read <- function(path) {
  if (!file.exists(path)) {
    stop(glue("File not found: {path}"))
  }
  readr::read_csv(path, show_col_types = FALSE)
}

# set data path relative to app.R (assumes R working dir is shiny/)
data_path <- "../data/data_fatal/"

# file paths
drivers_fp <- file.path(data_path, "fatal_accidents_drivers.csv")
marshals_fp <- file.path(data_path, "fatal_accidents_marshalls.csv")
redflags_fp <- file.path(data_path, "red_flags.csv")
safety_fp <- file.path(data_path, "safety_cars.csv")

# read with safe_read; wrap in tryCatch so app loads and shows helpful msg
drivers <- tryCatch(safe_read(drivers_fp), error = function(e) { message(e$message); NULL })
marshals <- tryCatch(safe_read(marshals_fp), error = function(e) { message(e$message); NULL })
redflags <- tryCatch(safe_read(redflags_fp), error = function(e) { message(e$message); NULL })
safety <- tryCatch(safe_read(safety_fp), error = function(e) { message(e$message); NULL })

# For robustness, try to discover column names we care about:
find_col <- function(df, candidates) {
  if (is.null(df)) return(NULL)
  cols <- tolower(names(df))
  for (c in candidates) {
    if (c %in% cols) return(names(df)[which(cols == c)[1]])
  }
  return(NULL)
}

# Common candidates
year_cols <- c("year", "race_year", "season")
country_cols <- c("country", "race_country", "location_country", "circuit_country")
constructor_cols <- c("constructor", "constructor_name", "team", "constructorId")
driver_cols <- c("driver", "driver_name", "driverid", "driver_name_full")
race_name_cols <- c("race", "race_name", "name")
date_cols <- c("date", "race_date", "r_date")
lat_cols <- c("lat", "latitude")
lon_cols <- c("lon", "lng", "longitude")

# guess columns for drivers dataset
drivers_year <- find_col(drivers, year_cols)
drivers_country <- find_col(drivers, country_cols)
drivers_constructor <- find_col(drivers, constructor_cols)
drivers_driver <- find_col(drivers, driver_cols)
drivers_race <- find_col(drivers, race_name_cols)
drivers_date <- find_col(drivers, date_cols)
drivers_lat <- find_col(drivers, lat_cols)
drivers_lon <- find_col(drivers, lon_cols)
drivers_status <- find_col(drivers, c("race_status", "status", "fatal", "fatality"))

# Helper: convert to a unified dataframe with standard names
normalize_drivers <- function(df) {
  if (is.null(df)) return(NULL)
  d <- df
  rename_map <- list()
  if (!is.null(drivers_year)) rename_map[[drivers_year]] <- "year"
  if (!is.null(drivers_country)) rename_map[[drivers_country]] <- "country"
  if (!is.null(drivers_constructor)) rename_map[[drivers_constructor]] <- "constructor"
  if (!is.null(drivers_driver)) rename_map[[drivers_driver]] <- "driver"
  if (!is.null(drivers_race)) rename_map[[drivers_race]] <- "race"
  if (!is.null(drivers_date)) rename_map[[drivers_date]] <- "race_date"
  if (!is.null(drivers_lat)) rename_map[[drivers_lat]] <- "lat"
  if (!is.null(drivers_lon)) rename_map[[drivers_lon]] <- "lon"
  if (!is.null(drivers_status)) rename_map[[drivers_status]] <- "race_status"
  for (orig in names(rename_map)) {
    names(d)[names(d) == orig] <- rename_map[[orig]]
  }
  if ("year" %in% names(d)) d$year <- as.integer(d$year)
  if ("race_status" %in% names(d)) d$race_status <- as.character(d$race_status)
  if ("lat" %in% names(d)) d$lat <- as.numeric(d$lat)
  if ("lon" %in% names(d)) d$lon <- as.numeric(d$lon)
  return(d)
}

drivers <- normalize_drivers(drivers)
marshals <- normalize_drivers(marshals)

# redflags: expect columns like reason, code, description
if (!is.null(redflags)) {
  rf_reason <- find_col(redflags, c("reason", "cause", "flag_reason"))
  if (!is.null(rf_reason)) {
    names(redflags)[names(redflags) == rf_reason] <- "reason"
  }
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("F1 Fatal Accidents — Interactive Dashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Year range:", min = 1950, max = 2025, value = c(1970, 2020), sep = ""),
      uiOutput("country_ui"),
      uiOutput("constructor_ui"),
      uiOutput("driver_ui"),
      selectInput("race_status", "Race Status:", choices = c("All", "fatal", "null"), selected = "All")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map & Trends",
                 h4("World Map (fatal vs non-fatal)"),
                 withSpinner(leafletOutput("map", height = 450)),
                 br(),
                 h4("Yearly Races / Fatal Counts"),
                 withSpinner(plotOutput("barline", height = 300))
        ),
        tabPanel("Age Comparison",
                 h4("Driver Age: Fatal vs Null"),
                 withSpinner(plotOutput("age_boxplot", height = 350))
        ),
        tabPanel("Causes",
                 h4("Fatal Accident Causes"),
                 withSpinner(plotOutput("cause_pie", height = 350))
        ),
        tabPanel("Driver Details",
                 h4("Selected Driver Summary"),
                 uiOutput("driver_info")
        ),
        tabPanel("Data Sources",
                 h4("Loaded CSV Data Sources"),
                 tags$ul(
                   tags$li(glue("Drivers: {drivers_fp}")),
                   tags$li(glue("Marshals: {marshals_fp}")),
                   tags$li(glue("Red Flags: {redflags_fp}")),
                   tags$li(glue("Safety Cars: {safety_fp}"))
                 )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # reactive dataset filtered by inputs
  filtered_drivers <- reactive({
    d <- drivers
    if (is.null(d)) return(NULL)
    if ("year" %in% names(d)) {
      yr_rng <- input$year_range
      d <- d %>% filter(is.na(year) | (year >= yr_rng[1] & year <= yr_rng[2]))
    }
    if (!is.null(input$country) && input$country != "All" && "country" %in% names(d)) {
      d <- d %>% filter(country == input$country)
    }
    if (!is.null(input$constructor) && input$constructor != "All" && "constructor" %in% names(d)) {
      d <- d %>% filter(constructor == input$constructor)
    }
    if (!is.null(input$driver) && input$driver != "All" && "driver" %in% names(d)) {
      d <- d %>% filter(driver == input$driver)
    }
    if (!is.null(input$race_status) && input$race_status != "All" && "race_status" %in% names(d)) {
      if (input$race_status == "fatal") {
        d <- d %>% filter(tolower(race_status) %in% c("fatal", "death", "died", "true", "1"))
      } else {
        d <- d %>% filter(!tolower(race_status) %in% c("fatal", "death", "died", "true", "1"))
      }
    }
    d
  })

  # dynamic UI choices
  output$country_ui <- renderUI({
    d <- drivers
    if (is.null(d) || !("country" %in% names(d))) {
      selectInput("country", "Country:", choices = c("All"))
    } else {
      choices <- c("All", sort(unique(na.omit(d$country))))
      selectInput("country", "Country:", choices = choices)
    }
  })
  output$constructor_ui <- renderUI({
    d <- drivers
    if (is.null(d) || !("constructor" %in% names(d))) {
      selectInput("constructor", "Constructor:", choices = c("All"))
    } else {
      choices <- c("All", sort(unique(na.omit(d$constructor))))
      selectInput("constructor", "Constructor:", choices = choices)
    }
  })
  output$driver_ui <- renderUI({
    d <- drivers
    if (is.null(d) || !("driver" %in% names(d))) {
      selectInput("driver", "Driver:", choices = c("All"))
    } else {
      choices <- c("All", sort(unique(na.omit(d$driver))))
      selectInput("driver", "Driver:", choices = choices)
    }
  })

  # Leaflet map
  output$map <- renderLeaflet({
    d <- filtered_drivers()
    if (is.null(d)) return(leaflet() %>% addTiles() %>% setView(0, 0, zoom = 2))
    if (!("lat" %in% names(d) && "lon" %in% names(d))) {
      leaflet() %>% addTiles() %>% setView(0, 0, zoom = 2)
    } else {
      d$st <- ifelse(tolower(as.character(d$race_status)) %in% c("fatal","death","died","true","1"), "fatal", "null")
      leaflet(d) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(~lon, ~lat, color = ~ifelse(st == "fatal", "red", "green"),
                         radius = 6, stroke = FALSE, fillOpacity = 0.8,
                         label = ~paste0(ifelse(!is.na(race), race, "Race"), " | ", ifelse(!is.na(race_date), race_date, "")))
    }
  })

  # Bar + line
  output$barline <- renderPlot({
    d <- filtered_drivers()
    if (is.null(d) || !("year" %in% names(d))) {
      ggplot() + labs(title = "No Year Column Found In Drivers Data")
    } else {
      df_year <- d %>%
        mutate(is_fatal = ifelse(tolower(as.character(race_status)) %in% c("fatal","death","died","true","1"), 1, 0)) %>%
        group_by(year) %>%
        summarize(total = n(), fatal_count = sum(is_fatal, na.rm = TRUE)) %>%
        arrange(year)
      ggplot(df_year, aes(x = year)) +
        geom_col(aes(y = total), fill = "grey80") +
        geom_line(aes(y = fatal_count), color = "red", size = 1) +
        geom_point(aes(y = fatal_count), color = "red", size = 1.5) +
        labs(y = "Count", x = "Year", title = "Total Races Per Year (bars) And Fatal Counts (line)")
    }
  })

  # Boxplot
  output$age_boxplot <- renderPlot({
    d <- filtered_drivers()
    if (is.null(d)) {
      ggplot() + labs(title = "Drivers Data Not Loaded")
    } else {
      age_col <- find_col(d, c("age", "driver_age"))
      if (is.null(age_col)) {
        ggplot() + labs(title = "No Driver Age Column Found")
      } else {
        d$st <- ifelse(tolower(as.character(d$race_status)) %in% c("fatal","death","died","true","1"), "Fatal", "Null")
        ggplot(d, aes(x = st, y = .data[[age_col]])) +
          geom_boxplot() +
          labs(x = "Race status", y = "Driver Age", title = "Driver Age: Fatal Vs Null")
      }
    }
  })

  # Pie chart
  output$cause_pie <- renderPlot({
    rf <- redflags
    if (is.null(rf)) {
      ggplot() + labs(title = "Red Flags Not Available")
    } else {
      reason_col <- find_col(rf, c("reason", "cause", "flag"))
      if (is.null(reason_col)) {
        ggplot() + labs(title = "No Reason Column Found In Red Flags")
      } else {
        p <- rf %>% group_by(.data[[reason_col]]) %>% summarize(n = n()) %>% arrange(desc(n))
        ggplot(p, aes(x = "", y = n, fill = .data[[reason_col]])) +
          geom_col(width = 1) +
          coord_polar(theta = "y") +
          theme_void() +
          labs(fill = "Cause", title = "Distribution Of Fatal Accident Causes")
      }
    }
  })

  # Driver info UI
  output$driver_info <- renderUI({
    sel <- input$driver
    if (is.null(sel) || sel == "All") return(HTML("<p>Select a driver to see summary.</p>"))
    d <- drivers %>% filter(driver == sel)
    if (nrow(d) == 0) return(HTML("<p>No data for selected driver.</p>"))
    age_col <- find_col(d, c("age", "driver_age"))
    status_col <- find_col(d, c("status", "driver_status", "died"))
    wins_col <- find_col(d, c("wins", "win_count", "total_wins"))
    tagList(
      tags$div(class="card p-3",
               tags$h5(sel),
               tags$p(HTML(paste0("<strong>Age:</strong> ", if(!is.null(age_col)) unique(d[[age_col]])[1] else "N/A"))),
               tags$p(HTML(paste0("<strong>Status:</strong> ", if(!is.null(status_col)) unique(d[[status_col]])[1] else "N/A"))),
               tags$p(HTML(paste0("<strong>Wins:</strong> ", if(!is.null(wins_col)) unique(d[[wins_col]])[1] else "N/A"))),
               tags$p(HTML(paste0("<strong>Accident reason (if any):</strong> ", if("race" %in% names(d)) paste(unique(d$race), collapse=", ") else "N/A")))
      )
    )
  })

}

shinyApp(ui, server)
