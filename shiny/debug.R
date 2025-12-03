library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(glue)
library(shinycssloaders)
library(scales)

#-----------------------------------
# SAFE READS
#-----------------------------------
safe_read <- function(path) {
  if (!file.exists(path)) stop(glue("File not found: {path}"))
  readr::read_csv(path, show_col_types = FALSE)
}

data_path <- "./data_fatal/"
drivers_fp <- file.path(data_path, "fatal_accidents_drivers.csv")
marshals_fp <- file.path(data_path, "fatal_accidents_marshalls.csv")
redflags_fp <- file.path(data_path, "red_flags.csv")
safety_fp <- file.path(data_path, "safety_cars.csv")

drivers <- tryCatch(safe_read(drivers_fp), error = function(e) { message(e$message); NULL })
marshals <- tryCatch(safe_read(marshals_fp), error = function(e) { message(e$message); NULL })
redflags <- tryCatch(safe_read(redflags_fp), error = function(e) { message(e$message); NULL })
safety <- tryCatch(safe_read(safety_fp), error = function(e) { message(e$message); NULL })

#-----------------------------------
# COLUMN FINDER
#-----------------------------------
find_col <- function(df, candidates) {
  if (is.null(df)) return(NULL)
  cols <- tolower(names(df))
  for (c in candidates) {
    if (c %in% cols) return(names(df)[which(cols == c)[1]])
  }
  return(NULL)
}

# Candidate names
year_cols <- c("year", "race_year", "season")
country_cols <- c("country", "race_country", "location_country", "circuit_country")
constructor_cols <- c("constructor", "constructor_name", "team", "constructorId")
driver_cols <- c("driver", "driver_name", "driverid", "driver_name_full")
race_name_cols <- c("race", "race_name", "name")
date_cols <- c("date", "race_date", "r_date")
lat_cols <- c("lat", "latitude")
lon_cols <- c("lon", "lng", "longitude")
age_cols <- c("age", "driver_age")
wins_cols <- c("wins", "driver_wins", "number_of_wins")
races_cols <- c("races", "number_of_races", "race_times")

#-----------------------------------
# NORMALIZATION
#-----------------------------------
drivers_year <- find_col(drivers, year_cols)
drivers_country <- find_col(drivers, country_cols)
drivers_constructor <- find_col(drivers, constructor_cols)
drivers_driver <- find_col(drivers, driver_cols)
drivers_race <- find_col(drivers, race_name_cols)
drivers_date <- find_col(drivers, date_cols)
drivers_lat <- find_col(drivers, lat_cols)
drivers_lon <- find_col(drivers, lon_cols)
drivers_status <- find_col(drivers, c("race_status","status","fatal","fatality"))
drivers_age <- find_col(drivers, age_cols)
drivers_wins <- find_col(drivers, wins_cols)
drivers_races <- find_col(drivers, races_cols)

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
  if (!is.null(drivers_age)) rename_map[[drivers_age]] <- "age"
  if (!is.null(drivers_wins)) rename_map[[drivers_wins]] <- "wins"
  if (!is.null(drivers_races)) rename_map[[drivers_races]] <- "races"

  for (o in names(rename_map)) names(d)[names(d) == o] <- rename_map[[o]]

  if ("year" %in% names(d)) d$year <- as.integer(d$year)
  if ("lat" %in% names(d)) d$lat <- as.numeric(d$lat)
  if ("lon" %in% names(d)) d$lon <- as.numeric(d$lon)
  return(d)
}

drivers <- normalize_drivers(drivers)
marshals <- normalize_drivers(marshals)

#-----------------------------------
# CLEAN RED FLAGS
#-----------------------------------
if (!is.null(redflags)) {
  rf_reason <- find_col(redflags, c("reason","cause","flag_reason"))
  if (!is.null(rf_reason)) names(redflags)[names(redflags) == rf_reason] <- "reason"

  rf_year_col <- find_col(redflags, year_cols)
  if (!is.null(rf_year_col) && rf_year_col != "year") names(redflags)[names(redflags) == rf_year_col] <- "year"
}

#-----------------------------------
# STATUS NORMALIZATION
#-----------------------------------
ensure_status <- function(df) {
  if (is.null(df)) return(NULL)

  if (!("race_status" %in% names(df))) {
    df$race_status <- "null"
  } else {
    df$race_status <- tolower(as.character(df$race_status))
    df$race_status[df$race_status %in% c("0","false","no","",NA)] <- "null"
    df$race_status[grepl("fatal|death|died|dead|true|1", df$race_status)] <- "fatal"
  }
  df
}

drivers <- ensure_status(drivers)
marshals <- ensure_status(marshals)

#-----------------------------------
# UI REDESIGN — LOOKS LIKE SKETCH
#-----------------------------------

ui <- fluidPage(

  tags$style(HTML("
    .card-box {
      background: #fff;
      padding: 15px;
      border-radius: 12px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      margin-bottom: 18px;
    }
    .title-section {
      font-weight: 700;
      font-size: 20px;
      margin-bottom: 10px;
    }
  ")),

  titlePanel("F1 Race Accidents Dashboard"),

  sidebarLayout(

    sidebarPanel(
      width = 3,
      sliderInput("year_range", "Year Range:",
                  min = 1950, max = 2025,
                  value = c(1980, 2020), sep = ""),

      uiOutput("country_ui"),
      uiOutput("constructor_ui"),
      uiOutput("driver_ui"),

      radioButtons("race_status", "Race Status:", 
                   choices = c("All","Fatal","Null"),
                   inline = TRUE),

      hr(),
      downloadButton("download_filtered_csv", "Download CSV")
    ),

    mainPanel(

      fluidRow(
        column(8,
               div(class="card-box",
                   div(class="title-section", "World Map (Fatal vs Non-Fatal)"),
                   withSpinner(leafletOutput("map", height = 420))
               )
        ),
        column(4,
               div(class="card-box",
                   div(class="title-section", "Wins by Year"),
                   withSpinner(plotOutput("barline", height = 420))
               )
        )
      ),

      fluidRow(
        column(4,
               div(class="card-box",
                   div(class="title-section", "Driver Age — Fatal vs Null"),
                   withSpinner(plotOutput("age_boxplot", height = 300))
               )
        ),
        column(4,
               div(class="card-box",
                   div(class="title-section", "Fatal Causes Breakdown"),
                   withSpinner(plotOutput("cause_pie", height = 300))
               )
        ),
        column(4,
               div(class="card-box",
                   div(class="title-section", "Driver Summary"),
                   withSpinner(uiOutput("driver_info"))
               )
        )
      )
    )
  )
)

#-----------------------------------
# SERVER
#-----------------------------------
server <- function(input, output, session) {

  output$country_ui <- renderUI({
    d <- drivers
    if (is.null(d)) return(selectInput("country","Country:",c("All")))
    selectInput("country","Country:",c("All",sort(unique(d$country))))
  })

  filtered_drivers <- reactive({
    d <- drivers
    if (is.null(d)) return(NULL)

    d <- d %>% filter(year >= input$year_range[1] & year <= input$year_range[2])

    if (input$country != "All") d <- d %>% filter(country == input$country)
    if (input$constructor != "All") d <- d %>% filter(constructor == input$constructor)
    if (input$driver != "All") d <- d %>% filter(driver == input$driver)

    if (input$race_status == "Fatal") d <- d %>% filter(race_status == "fatal")
    if (input$race_status == "Null")  d <- d %>% filter(race_status == "null")

    d$color <- ifelse(d$race_status == "fatal", "red", "green")

    d
  })

  output$constructor_ui <- renderUI({
    d <- drivers
    selectInput("constructor","Constructor:",c("All",sort(unique(d$constructor))))
  })

  output$driver_ui <- renderUI({
    d <- drivers
    selectInput("driver","Driver:",c("All",sort(unique(d$driver))))
  })

  output$map <- renderLeaflet({
    d <- filtered_drivers()
    if (is.null(d)) return(leaflet() %>% addTiles())

    leaflet(d) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(~lon, ~lat, color=~color, radius=6, fillOpacity=0.7) %>%
      addLegend("bottomright", colors=c("red","green"),
                labels=c("Fatal","Null"), title="Race Outcome")
  })

  output$barline <- renderPlot({
    d <- filtered_drivers()
    if (is.null(d)) return(NULL)

    plot_data <- d %>%
      group_by(year) %>%
      summarize(total_races=n(),
                fatal=sum(race_status=="fatal"))

    ggplot(plot_data, aes(year)) +
      geom_col(aes(y=total_races), fill="gray80") +
      geom_line(aes(y=fatal * max(total_races)/max(fatal)), color="red", size=1.1) +
      scale_y_continuous(name="Total Races",
                         sec.axis = sec_axis(~ . * max(fatal)/max(total_races),
                                             name="Fatalities")) +
      theme_minimal() +
      labs(title="Races & Fatalities by Year")
  })

  output$age_boxplot <- renderPlot({
    d <- filtered_drivers()
    if (is.null(d) || !"age" %in% names(d)) return(NULL)

    ggplot(d, aes(race_status, age, fill=race_status)) +
      geom_boxplot() +
      scale_fill_manual(values=c("fatal"="red","null"="green")) +
      theme_minimal() +
      labs(title="Age Comparison", x="", y="Age")
  })

  output$cause_pie <- renderPlot({
    if (is.null(redflags)) return(NULL)

    rf <- redflags %>% filter(year >= input$year_range[1] & year <= input$year_range[2])

    ggplot(rf, aes(x="", fill=reason)) +
      geom_bar() +
      coord_polar("y") +
      theme_void() +
      labs(title="Red Flag Causes")
  })

  output$driver_info <- renderUI({
    d <- filtered_drivers()
    if (is.null(d) || input$driver == "All") return(p("Select a driver."))

    dd <- d %>% filter(driver == input$driver)
    if (nrow(dd) == 0) return(p("No driver data."))

    total <- nrow(dd)
    fat <- sum(dd$race_status=="fatal")
    alive <- total - fat

    avg_age <- round(mean(dd$age, na.rm=TRUE),1)

    div(
      h4(input$driver),
      p(strong("Avg Age: "), avg_age),
      p(strong("Total Races: "), total),
      p(strong("Fatal Races: "), fat),
      p(strong("Alive/Null: "), alive)
    )
  })

  output$download_filtered_csv <- downloadHandler(
    filename=function(){ paste0("drivers_",Sys.Date(),".csv") },
    content=function(file){
      write.csv(filtered_drivers(), file, row.names=FALSE)
    }
  )
}

shinyApp(ui, server)
