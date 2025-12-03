library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(glue)
library(shinycssloaders)
library(lubridate)
library(stringr)


safe_read <- function(path) {
  if (!file.exists(path)) stop(glue("File not found: {path}"))
  read_csv(path, show_col_types = FALSE)
}


data_fatal_path <- "./data_fatal/"
data_origin_path <- "./data_origin/"


drivers_fatal_fp <- file.path(data_fatal_path, "fatal_accidents_drivers.csv")
marshals_fp <- file.path(data_fatal_path, "fatal_accidents_marshalls.csv")
redflags_fp <- file.path(data_fatal_path, "red_flags.csv")
safety_fp <- file.path(data_fatal_path, "safety_cars.csv")


drivers_fp <- file.path(data_origin_path, "drivers.csv")
constructors_fp <- file.path(data_origin_path, "constructors.csv")
races_fp <- file.path(data_origin_path, "races.csv")
results_fp <- file.path(data_origin_path, "results.csv")
driver_standings_fp <- file.path(data_origin_path, "driver_standings.csv")


drivers_fatal <- tryCatch(safe_read(drivers_fatal_fp), error = function(e) { message(e$message); NULL })
drivers <- tryCatch(safe_read(drivers_fp), error = function(e) { message(e$message); NULL })
constructors <- tryCatch(safe_read(constructors_fp), error = function(e) { message(e$message); NULL })
races <- tryCatch(safe_read(races_fp), error = function(e) { message(e$message); NULL })
results <- tryCatch(safe_read(results_fp), error = function(e) { message(e$message); NULL })
driver_standings <- tryCatch(safe_read(driver_standings_fp), error = function(e) { message(e$message); NULL })
redflags <- tryCatch(safe_read(redflags_fp), error = function(e) { message(e$message); NULL })


normalize_names <- function(df) {
  if (is.null(df)) return(NULL)
  names(df) <- tolower(gsub("\\s+","_",names(df)))
  df
}

drivers_fatal <- normalize_names(drivers_fatal)
drivers <- normalize_names(drivers)
constructors <- normalize_names(constructors)
races <- normalize_names(races)
results <- normalize_names(results)
driver_standings <- normalize_names(driver_standings)
redflags <- normalize_names(redflags)


if (!is.null(drivers_fatal) && nrow(drivers_fatal) > 0) {
  drivers_fatal <- drivers_fatal %>%
    mutate(fullname = str_to_lower(driver)) %>%
    select(fullname, date_of_accident, event, car, session)
}

if (!is.null(drivers)) {
  drivers <- drivers %>%
    mutate(fullname = str_to_lower(paste(forename, surname))) %>%
    left_join(
      drivers_fatal %>% mutate(race_status = "fatal") %>% select(fullname, race_status),
      by = "fullname"
    ) %>%
    mutate(race_status = ifelse(is.na(race_status), "null", race_status))
}


ui <- fluidPage(
  titlePanel("F1 Fatal Accidents Dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("year_range","Driver DOB Year:",min=1900,max=2025,value=c(1980,2000)),
      uiOutput("country_ui"),
      uiOutput("constructor_ui"),
      uiOutput("driver_ui"),
      radioButtons("race_status","Race Status:",choices=c("All","Fatal","Null"), inline=TRUE)
    ),
    mainPanel(
      fluidRow(
        column(6, withSpinner(leafletOutput("map", height=400))),
        column(6, withSpinner(plotOutput("barline", height=400)))
      ),
      fluidRow(
        column(4, withSpinner(plotOutput("age_boxplot", height=300))),
        column(4, withSpinner(plotOutput("cause_pie", height=300))),
        column(4, withSpinner(uiOutput("driver_info")))
      )
    )
  )
)


server <- function(input, output, session) {

  output$country_ui <- renderUI({
    if (is.null(drivers) || !"nationality" %in% names(drivers)) return(selectInput("country","Country:",c("All")))
    selectInput("country","Country:",c("All",sort(unique(drivers$nationality))))
  })

  output$constructor_ui <- renderUI({
    if (is.null(constructors) || !"name" %in% names(constructors)) return(selectInput("constructor","Constructor:",c("All")))
    selectInput("constructor","Constructor:",c("All",sort(unique(constructors$name))))
  })

  output$driver_ui <- renderUI({
    if (is.null(drivers) || !"fullname" %in% names(drivers)) return(selectInput("driver","Driver:",c("All")))
    selectInput("driver","Driver:",c("All",sort(drivers$fullname)))
  })

 
  filtered_drivers <- reactive({
    d <- drivers
    if (is.null(d)) return(NULL)
    
    if ("dob" %in% names(d) && !all(is.na(d$dob))) {
      d <- d %>% filter(!is.na(dob) & year(ymd(dob)) >= input$year_range[1] & year(ymd(dob)) <= input$year_range[2])
    }
    if (!is.null(input$country) && input$country != "All") d <- d %>% filter(nationality == input$country)
    if (!is.null(input$driver) && input$driver != "All") d <- d %>% filter(fullname == input$driver)
    if (!is.null(input$race_status)) {
      if (input$race_status == "Fatal") d <- d %>% filter(race_status=="fatal")
      if (input$race_status == "Null") d <- d %>% filter(race_status=="null")
    }
    d
  })

  
  output$map <- renderLeaflet({
    d <- filtered_drivers()
    if (is.null(d) || !"lat" %in% names(d) || !"lng" %in% names(d)) return(leaflet() %>% addTiles())
    leaflet(d) %>%
      addTiles() %>%
      addCircleMarkers(~lng,~lat,color=~ifelse(race_status=="fatal","red","green"),radius=5)
  })

  
  output$barline <- renderPlot({
    d <- filtered_drivers()
    if (is.null(d) || nrow(d)==0) return(NULL)
    d %>% group_by(fullname, race_status) %>%
      summarise(races=n(), .groups="drop") %>%
      ggplot(aes(x=fullname, y=races, fill=race_status)) +
      geom_col() +
      theme_minimal() +
      coord_flip() +
      labs(title="Races by Driver", x="", y="Races")
  })

 
  output$age_boxplot <- renderPlot({
    d <- filtered_drivers()
    if (is.null(d) || !"dob" %in% names(d) || nrow(d)==0) return(NULL)
    d$age <- year(Sys.Date()) - year(ymd(d$dob))
    ggplot(d, aes(x=race_status, y=age, fill=race_status)) +
      geom_boxplot() +
      scale_fill_manual(values=c("fatal"="red","null"="green")) +
      labs(title="Driver Age by Race Outcome")
  })

 
  output$cause_pie <- renderPlot({
    if (is.null(redflags) || !"incident" %in% names(redflags) || nrow(redflags)==0) return(NULL)
    ggplot(redflags, aes(x="", fill=incident)) +
      geom_bar() +
      coord_polar("y") +
      theme_void() +
      labs(title="Red Flag Causes")
  })

  
  output$driver_info <- renderUI({
    d <- filtered_drivers()
    if (is.null(d) || nrow(d)==0 || is.null(input$driver) || input$driver %in% c("", "All")) {
      return(p("Select a driver to see summary."))
    }
    dd <- d
    total <- nrow(dd)
    fatal <- sum(dd$race_status=="fatal")
    alive <- total - fatal
    avg_age <- round(mean(year(Sys.Date())-year(ymd(dd$dob)), na.rm=TRUE),1)
    div(
      h4(str_to_title(input$driver)),
      p(strong("Average Age: "), avg_age),
      p(strong("Total Races: "), total),
      p(strong("Fatal Races: "), fatal),
      p(strong("Alive/Null: "), alive)
    )
  })
}


shinyApp(ui, server)
