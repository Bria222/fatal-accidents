library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(glue)
library(shinycssloaders)
library(scales) 


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

find_col <- function(df, candidates) {
  if (is.null(df)) return(NULL)
  cols <- tolower(names(df))
  for (c in candidates) {
    if (c %in% cols) return(names(df)[which(cols == c)[1]])
  }
  return(NULL)
}


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


drivers_year <- find_col(drivers, year_cols)
drivers_country <- find_col(drivers, country_cols)
drivers_constructor <- find_col(drivers, constructor_cols)
drivers_driver <- find_col(drivers, driver_cols)
drivers_race <- find_col(drivers, race_name_cols)
drivers_date <- find_col(drivers, date_cols)
drivers_lat <- find_col(drivers, lat_cols)
drivers_lon <- find_col(drivers, lon_cols)
drivers_status <- find_col(drivers, c("race_status", "status", "fatal", "fatality"))
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


if (!is.null(redflags)) {
  rf_reason <- find_col(redflags, c("reason", "cause", "flag_reason"))
  if (!is.null(rf_reason)) {
    names(redflags)[names(redflags) == rf_reason] <- "reason"
  }
  rf_year_col <- find_col(redflags, year_cols)
  if (!is.null(rf_year_col) && rf_year_col != "year") {
    names(redflags)[names(redflags) == rf_year_col] <- "year"
  }
}


ensure_status <- function(df) {
  if (is.null(df)) return(NULL)
  if (!("race_status" %in% names(df))) {
    message("No race_status-like column found. Creating default 'null' values.")
    df$race_status <- "null"
  } else {
    df$race_status <- tolower(as.character(df$race_status))
    
    df$race_status[df$race_status %in% c("0", "false", "no", "", NA)] <- "null"
    df$race_status[grepl("fatal|death|died|dead|true|1", df$race_status, ignore.case = TRUE)] <- "fatal"
    df$race_status[is.na(df$race_status)] <- "null"
  }
  return(df)
}

drivers <- ensure_status(drivers)
marshals <- ensure_status(marshals)


ui <- fluidPage(
  titlePanel("Interactive Dashboard"),
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("year_range", "Year range:", min = 1950, max = 2025, value = c(1970, 2020), sep = ""),
      
      
      uiOutput("country_ui"),
      
      
      uiOutput("constructor_ui"),
      
      
      uiOutput("driver_ui"),
      
      
      radioButtons("race_status", "Race Status:", choices = c("All" = "All", "Fatal" = "fatal", "Null" = "null"), selected = "All", inline = TRUE),
      
      hr(),
      downloadButton("download_filtered_csv", "Download filtered data (CSV)")
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
        
      )
    )
  )
)


server <- function(input, output, session) {
  

  output$country_ui <- renderUI({
    d <- drivers
    if (is.null(d) || !("country" %in% names(d))) {
      selectInput("country", "Country:", choices = c("All", "None"), selected = "All")
    } else {
      choices <- c("All", "None", sort(unique(na.omit(d$country))))
      selectInput("country", "Country:", choices = choices, selected = "All")
    }
  })
  
  
  filtered_drivers <- reactive({
    d <- drivers
    if (is.null(d)) return(NULL)
    
    
    if ("year" %in% names(d)) {
      yr_rng <- input$year_range
      d <- d %>% filter(is.na(year) | (year >= yr_rng[1] & year <= yr_rng[2]))
    }
    
   
    if (!is.null(input$country) && "country" %in% names(d)) {
      if (input$country == "None") {
        d <- d[0,]  
      } else if (input$country != "All") {
        d <- d %>% filter(country == input$country)
      }
    }
    
   
    if (!is.null(input$constructor) && input$constructor != "All" && "constructor" %in% names(d)) {
      d <- d %>% filter(constructor == input$constructor)
    }
    
   
    if (!is.null(input$driver) && input$driver != "All" && "driver" %in% names(d)) {
      d <- d %>% filter(driver == input$driver)
    }
    
    
    if (!is.null(input$race_status) && input$race_status != "All" && "race_status" %in% names(d)) {
      if (input$race_status == "fatal") {
        d <- d %>% filter(tolower(race_status) %in% c("fatal"))
      } else if (input$race_status == "null") {
        d <- d %>% filter(!tolower(race_status) %in% c("fatal"))
      }
    }
    
    
    if ("race_status" %in% names(d)) {
      d <- d %>%
        mutate(
          st = case_when(
            grepl("fatal|death|died|dead", race_status, ignore.case = TRUE) ~ "fatal",
            TRUE ~ "null"
          ),
          color = ifelse(st == "fatal", "red", "green")
        )
    } else {
      d$st <- "null"
      d$color <- "green"
    }
    
    d
  })
  
 
  output$constructor_ui <- renderUI({
    d <- drivers
    if (is.null(d) || !("constructor" %in% names(d))) {
      selectInput("constructor", "Constructor:", choices = c("All"))
    } else {
      choices <- c("All", sort(unique(na.omit(d$constructor))))
      selectInput("constructor", "Constructor:", choices = choices, selected = "All")
    }
  })
  
  output$driver_ui <- renderUI({
    d <- drivers
    if (is.null(d) || !("driver" %in% names(d))) {
      selectInput("driver", "Driver:", choices = c("All"))
    } else {
      choices <- c("All", sort(unique(na.omit(d$driver))))
      selectInput("driver", "Driver:", choices = choices, selected = "All")
    }
  })
  
  
  output$map <- renderLeaflet({
    d <- filtered_drivers()
    if (is.null(d)) return(leaflet() %>% addTiles() %>% setView(0, 0, zoom = 2))
    
    if (!("lat" %in% names(d) && "lon" %in% names(d))) {
      leaflet() %>% addTiles() %>% setView(0, 0, zoom = 2)
    } else {
      leaflet(d) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(~lon, ~lat,
                         color = ~color,
                         radius = 6,
                         stroke = FALSE,
                         fillOpacity = 0.7,
                         popup = ~glue("<b>Race:</b> {race}<br><b>Driver:</b> {driver}<br><b>Status:</b> {race_status}")) %>%
        addLegend("bottomright", colors = c("red", "green"), labels = c("Fatal", "Null"),
                  title = "Race Outcome", opacity = 1)
    }
  })
  
 
  output$barline <- renderPlot({
    d <- filtered_drivers()
    if (is.null(d) || !("year" %in% names(d))) return(ggplot() + labs(title = "Insufficient data for yearly trends"))
    
    plot_data <- d %>%
      group_by(year) %>%
      summarise(total_races = n(),
                fatal_count = sum(tolower(race_status) %in% c("fatal"), na.rm = TRUE),
                .groups = "drop") %>%
      arrange(year)
    
    if (nrow(plot_data) == 0) return(ggplot() + labs(title = "No data in selected year range"))
    
    max_races <- max(plot_data$total_races, na.rm = TRUE)
    max_fatal <- max(plot_data$fatal_count, na.rm = TRUE)
    
    if (max_fatal == 0) {
      ggplot(plot_data, aes(x = year)) +
        geom_col(aes(y = total_races), fill = "gray", alpha = 0.7) +
        labs(title = "Races by Year (no fatalities in selection)", x = "Year", y = "# Races") +
        theme_minimal()
    } else {
      scale_factor <- max_races / max_fatal
      ggplot(plot_data, aes(x = year)) +
        geom_col(aes(y = total_races), stat = "identity", fill = "gray", alpha = 0.6) +
        geom_line(aes(y = fatal_count * scale_factor), color = "red", size = 1) +
        scale_y_continuous(
          name = "# Races",
          sec.axis = sec_axis(~ . / scale_factor, name = "# Fatalities")
        ) +
        labs(title = "Races and Fatalities by Year", x = "Year") +
        theme_minimal()
    }
  })
  
  
  output$age_boxplot <- renderPlot({
    d <- filtered_drivers()
    if (is.null(d) || !("age" %in% names(d))) return(ggplot() + labs(title = "Insufficient age data"))
    
    ggplot(d, aes(x = st, y = age, fill = st)) +
      geom_boxplot() +
      labs(title = "Driver Age Comparison: Fatal vs Null Outcomes", x = "Outcome", y = "Age") +
      scale_fill_manual(values = c("fatal" = "red", "null" = "green"), na.value = "gray") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  
  output$cause_pie <- renderPlot({
    if (is.null(redflags)) return(ggplot() + labs(title = "Red flags data not available"))
    
    rf_filtered <- redflags
    if ("year" %in% names(rf_filtered)) {
      rf_filtered <- rf_filtered %>% filter(is.na(year) | (year >= input$year_range[1] & year <= input$year_range[2]))
    }
    
    if (!("reason" %in% names(rf_filtered))) return(ggplot() + labs(title = "Reason column not found in red flags data"))
    
    cause_counts <- rf_filtered %>%
      group_by(reason) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    ggplot(cause_counts, aes(x = "", y = count, fill = reason)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      labs(title = "Fatal Accident Causes Breakdown", fill = "Reason") +
      theme_void()
  })
  

  output$driver_info <- renderUI({
    d <- filtered_drivers()
    selected_driver_name <- input$driver
    
    if (is.null(d) || selected_driver_name == "All" || !("driver" %in% names(d))) {
      return(p("Select a specific driver from the sidebar to view details."))
    }
    
    driver_data <- d %>% filter(driver == selected_driver_name)
    
    if (nrow(driver_data) == 0) {
      return(p("No data available for the selected driver in this year range."))
    }
    
    total_races <- nrow(driver_data)
    fatal_races <- sum(tolower(driver_data$race_status) %in% c("fatal"), na.rm = TRUE)
    alive_races <- total_races - fatal_races
    
    avg_age <- if ("age" %in% names(driver_data)) round(mean(driver_data$age, na.rm = TRUE), 1) else NA
    total_wins <- if ("wins" %in% names(driver_data)) sum(driver_data$wins, na.rm = TRUE) else NA
    fatal_rate <- if (total_races > 0) percent(fatal_races / total_races) else "N/A"
    
    div(
      h4(selected_driver_name),
      fluidRow(
        column(4, wellPanel(h5("Avg Age"), p(avg_age), style = "background-color: #f2f2f2;")),
        column(4, wellPanel(h5("Total Wins"), p(total_wins), style = "background-color: #e6e6e6;")),
        column(4, wellPanel(h5("Fatal Rate"), p(fatal_rate), style = "background-color: #ffcccc;"))
      ),
      h5("Race Outcomes"),
      fluidRow(
        column(6, wellPanel(h5("Alive/Null"), p(alive_races), style = "background-color: #ccffcc;")),
        column(6, wellPanel(h5("Died/Fatal"), p(fatal_races), style = "background-color: #ff9999;"))
      )
    )
  })
  
 
  output$download_filtered_csv <- downloadHandler(
    filename = function() {
      paste0("drivers_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      d <- filtered_drivers()
      if (is.null(d)) {
        write.csv(data.frame(), file, row.names = FALSE)
      } else {
        write.csv(d, file, row.names = FALSE)
      }
    }
  )
}


shinyApp(ui = ui, server = server)
