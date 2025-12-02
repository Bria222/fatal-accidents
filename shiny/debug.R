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

data_path <- "../data/data_fatal/"
drivers_fp <- file.path(data_path, "fatal_accidents_drivers.csv")
marshals_fp <- file.path(data_path, "fatal_accidents_marshalls.csv")
redflags_fp <- file.path(data_path, "red_flags.csv")
safety_fp <- file.path(data_path, "safety_cars.csv")

drivers <- tryCatch(safe_read(drivers_fp), error = function(e) { message(e$message); NULL })
marshals <- tryCatch(safe_read(marshals_fp), error = function(e) { message(e$message); NULL })
redflags <- tryCatch(safe_read(redflags_fp), error = function(e) { message(e$message); NULL })
safety <- tryCatch(safe_read(safety_fp), error = function(e) { message(e$message); NULL })


normalize_df <- function(df, mapping) {
  if (is.null(df)) return(NULL)
  for (col in names(mapping)) {
    if (mapping[[col]] %in% names(df)) names(df)[names(df) == mapping[[col]]] <- col
  }
  return(df)
}

drivers <- normalize_df(drivers, list(
  driver = "Driver",
  constructor = "Car",
  race = "Event",
  race_date = "Date Of Accident",
  age = "Age"
))
marshals <- normalize_df(marshals, list(
  driver = "Name",
  race = "Event",
  race_date = "Date Of Accident",
  age = "Age"
))


ensure_status <- function(df) {
  if (is.null(df)) return(NULL)
  if (!("race_status" %in% names(df))) df$race_status <- "null"
  df$race_status <- tolower(as.character(df$race_status))
  df$race_status[grepl("fatal|death|died|dead", df$race_status, ignore.case = TRUE)] <- "fatal"
  df$race_status[is.na(df$race_status) | df$race_status == ""] <- "null"
  df
}
drivers <- ensure_status(drivers)
marshals <- ensure_status(marshals)

ui <- fluidPage(
  titlePanel("F1 Fatal Accidents — Interactive Dashboard"),
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
                 withSpinner(plotOutput("cause_pie", height = 350)),
                 h4("Safety Cars / Red Flags"),
                 withSpinner(plotOutput("safety_redflags_plot", height = 350))
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


server <- function(input, output, session) {

 
  output$country_ui <- renderUI({
    countries <- c("All", sort(unique(c(drivers$race, marshals$race))))
    selectInput("country", "Country:", choices = countries, selected = "All")
  })
  output$constructor_ui <- renderUI({
    choices <- c("All", sort(unique(na.omit(drivers$constructor))))
    selectInput("constructor", "Constructor:", choices = choices, selected = "All")
  })
  output$driver_ui <- renderUI({
    choices <- c("All", sort(unique(na.omit(drivers$driver))))
    selectInput("driver", "Driver:", choices = choices, selected = "All")
  })

  
  filtered_drivers <- reactive({
    d <- drivers
    if (!is.null(d)) {
      if (!is.null(input$year_range) && "race_date" %in% names(d)) {
        d <- d %>% filter(as.numeric(substr(race_date, nchar(race_date)-3, nchar(race_date))) >= input$year_range[1] &
                            as.numeric(substr(race_date, nchar(race_date)-3, nchar(race_date))) <= input$year_range[2])
      }
      if (!is.null(input$country) && input$country != "All") d <- d %>% filter(race == input$country)
      if (!is.null(input$constructor) && input$constructor != "All") d <- d %>% filter(constructor == input$constructor)
      if (!is.null(input$driver) && input$driver != "All") d <- d %>% filter(driver == input$driver)
      if (!is.null(input$race_status) && input$race_status != "All") d <- d %>% filter(race_status == input$race_status)
      d$color <- ifelse(d$race_status == "fatal", "red", "green")
    }
    d
  })

  
  output$map <- renderLeaflet({
    df <- filtered_drivers()
    m <- leaflet() %>% addTiles() %>% setView(0, 0, zoom = 2)
    if (!is.null(df) && all(c("lat","lon") %in% names(df))) {
      m <- m %>% addCircleMarkers(data = df, ~lon, ~lat, color = ~color, radius = 6, stroke = FALSE, fillOpacity = 0.7,
                                  popup = ~glue("<b>Race:</b> {race}<br><b>Driver:</b> {driver}<br><b>Status:</b> {race_status}"))
    }
    if (!is.null(marshals) && all(c("lat","lon") %in% names(marshals))) {
      m <- m %>% addCircleMarkers(data = marshals, ~lon, ~lat, color = "blue", radius = 5, stroke = FALSE, fillOpacity = 0.5,
                                  popup = ~glue("<b>Marshal:</b> {driver}<br><b>Race:</b> {race}<br><b>Status:</b> {race_status}"))
    }
    m %>% addLegend("bottomright", colors = c("red", "green", "blue"), labels = c("Fatal Driver","Null Driver","Marshal"),
                    title = "Race Outcome", opacity = 1)
  })

  
  output$barline <- renderPlot({
    df <- filtered_drivers()
    if (!is.null(df)) {
      df$year <- as.numeric(substr(df$race_date, nchar(df$race_date)-3, nchar(df$race_date)))
      plot_df <- df %>% group_by(year) %>% summarise(total = n(), fatal = sum(race_status=="fatal", na.rm=TRUE))
      ggplot(plot_df, aes(x=year)) +
        geom_col(aes(y=total), fill="gray", alpha=0.6) +
        geom_line(aes(y=fatal*max(total)/max(fatal)), color="red", size=1) +
        scale_y_continuous(name="# Races", sec.axis=sec_axis(~./(max(total)/max(fatal)), name="# Fatalities")) +
        labs(title="Races and Fatalities by Year") + theme_minimal()
    }
  })

  
  output$age_boxplot <- renderPlot({
    df <- filtered_drivers()
    if (!is.null(df) && "age" %in% names(df)) {
      ggplot(df, aes(x=race_status, y=age, fill=race_status)) +
        geom_boxplot() + scale_fill_manual(values=c("fatal"="red","null"="green")) +
        labs(title="Driver Age Comparison: Fatal vs Null Outcomes", x="Outcome", y="Age") +
        theme_minimal() + theme(legend.position="none")
    }
  })

  
  output$cause_pie <- renderPlot({
    if (!is.null(redflags)) {
      rf <- redflags
      if ("year" %in% names(rf)) rf <- rf %>% filter(year >= input$year_range[1] & year <= input$year_range[2])
      rf <- rf %>% group_by(Cause=Cause) %>% summarise(count=n(), .groups="drop")
      ggplot(rf, aes(x="", y=count, fill=Cause)) +
        geom_bar(stat="identity", width=1) + coord_polar("y") +
        labs(title="Red Flags Breakdown", fill="Cause") + theme_void()
    }
  })

 
  output$safety_redflags_plot <- renderPlot({
    if (!is.null(safety)) {
      sc <- safety
      sc$year <- as.numeric(substr(sc$Race, nchar(sc$Race)-3, nchar(sc$Race)))
      sc <- sc %>% filter(year >= input$year_range[1] & year <= input$year_range[2])
      sc_count <- sc %>% group_by(year) %>% summarise(safety_cars = n(), .groups="drop")
      if (!is.null(redflags)) {
        rf <- redflags
        rf$year <- as.numeric(substr(rf$Race, nchar(rf$Race)-3, nchar(rf$Race)))
        rf <- rf %>% filter(year >= input$year_range[1] & year <= input$year_range[2])
        rf_count <- rf %>% group_by(year) %>% summarise(red_flags = n(), .groups="drop")
        plot_df <- full_join(sc_count, rf_count, by="year") %>% tidyr::replace_na(list(safety_cars=0, red_flags=0))
        ggplot(plot_df, aes(x=year)) +
          geom_line(aes(y=safety_cars, color="Safety Cars"), size=1) +
          geom_line(aes(y=red_flags, color="Red Flags"), size=1) +
          scale_color_manual(values=c("Safety Cars"="blue","Red Flags"="orange")) +
          labs(title="Safety Cars & Red Flags by Year", y="Count", x="Year", color="Legend") + theme_minimal()
      }
    }
  })

  
  output$driver_info <- renderUI({
    df <- filtered_drivers()
    driver_name <- input$driver
    if (is.null(df) || driver_name=="All") return(p("Select a specific driver from the sidebar."))
    driver_data <- df %>% filter(driver==driver_name)
    if (nrow(driver_data)==0) return(p("No data for selected driver in this range."))
    total <- nrow(driver_data)
    fatal <- sum(driver_data$race_status=="fatal")
    alive <- total - fatal
    avg_age <- if ("age" %in% names(driver_data)) round(mean(driver_data$age, na.rm=TRUE),1) else NA
    fatal_rate <- percent(fatal/total)
    div(
      h4(driver_name),
      fluidRow(
        column(4, wellPanel(h5("Avg Age"), p(avg_age))),
        column(4, wellPanel(h5("Total Races"), p(total))),
        column(4, wellPanel(h5("Fatal Rate"), p(fatal_rate)))
      ),
      h5("Race Outcomes"),
      fluidRow(
        column(6, wellPanel(h5("Alive/Null"), p(alive))),
        column(6, wellPanel(h5("Died/Fatal"), p(fatal)))
      )
    )
  })

 
  output$download_filtered_csv <- downloadHandler(
    filename = function() { paste0("drivers_filtered_", Sys.Date(), ".csv") },
    content = function(file) {
      d <- filtered_drivers()
      write.csv(d %||% data.frame(), file, row.names=FALSE)
    }
  )

}


shinyApp(ui=ui, server=server)
