# app.R
# F1 Fatal Accidents Dashboard — hand-drawn layout reproduction
# Expects (at minimum): data_origin/results.csv, races.csv, constructors.csv
# Optional: data_origin/drivers.csv, data_fatal/fatal_accidents_drivers.csv, data_fatal/red_flags.csv

library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(glue)
library(shinycssloaders)
library(lubridate)
library(stringr)
library(tidyr)

# ----------------- Helper: safe read -----------------
safe_read <- function(path) {
  if (!file.exists(path)) {
    message(glue("File not found: {path}"))
    return(NULL)
  }
  read_csv(path, show_col_types = FALSE)
}

normalize_names <- function(df) {
  if (is.null(df)) return(NULL)
  names(df) <- tolower(gsub("\\s+","_",names(df)))
  df
}

# ----------------- File paths (edit if needed) -----------------
data_origin_path <- "./data_origin"
data_fatal_path  <- "./data_fatal"

results_fp        <- file.path(data_origin_path, "results.csv")
races_fp          <- file.path(data_origin_path, "races.csv")
constructors_fp   <- file.path(data_origin_path, "constructors.csv")
drivers_fp        <- file.path(data_origin_path, "drivers.csv")

drivers_fatal_fp  <- file.path(data_fatal_path, "fatal_accidents_drivers.csv")
redflags_fp       <- file.path(data_fatal_path, "red_flags.csv")

# ----------------- Load data -----------------
results      <- safe_read(results_fp) %>% normalize_names()
races        <- safe_read(races_fp) %>% normalize_names()
constructors <- safe_read(constructors_fp) %>% normalize_names()
drivers      <- safe_read(drivers_fp) %>% normalize_names()
drivers_fatal<- safe_read(drivers_fatal_fp) %>% normalize_names()
redflags     <- safe_read(redflags_fp) %>% normalize_names()

# ----------------- Basic normalization & derived columns -----------------
# make sure key columns exist and are the expected types
if (!is.null(results)) {
  # ensure position/positionorder exist
  if (!"positionorder" %in% names(results) & "position" %in% names(results)) {
    results <- results %>% mutate(positionorder = suppressWarnings(as.integer(position)))
  }
}

if (!is.null(races)) {
  # ensure year numeric
  if ("year" %in% names(races)) races <- races %>% mutate(year = as.integer(year))
}

if (!is.null(constructors)) {
  if (!"name" %in% names(constructors) & "constructorref" %in% names(constructors)) {
    constructors <- constructors %>% rename(name = constructorref)
  }
}

# Build a human driver name if possible (drivers may have forename/surname)
if (!is.null(drivers)) {
  if (!"fullname" %in% names(drivers)) {
    if (all(c("forename","surname") %in% names(drivers))) {
      drivers <- drivers %>% mutate(fullname = str_to_lower(str_trim(paste(forename, surname))))
    } else if ("name" %in% names(drivers)) {
      drivers <- drivers %>% mutate(fullname = str_to_lower(name))
    } else {
      drivers$fullname <- NA_character_
    }
  } else {
    drivers <- drivers %>% mutate(fullname = str_to_lower(fullname))
  }
}

# Attach race_status to drivers using drivers_fatal if possible
if (!is.null(drivers) && !is.null(drivers_fatal)) {
  # drivers_fatal likely has a "driver" or "fullname" or similar column and date_of_accident
  name_col <- intersect(c("fullname","driver","name"), names(drivers_fatal))[1]
  if (!is.na(name_col)) {
    df_f <- drivers_fatal %>% mutate(driver_id_name = str_to_lower(!!sym(name_col)))
    drivers <- drivers %>% mutate(driver_id_name = str_to_lower(ifelse(is.na(fullname), "", fullname)))
    drivers <- left_join(drivers, df_f %>% mutate(race_status = "fatal") %>% select(driver_id_name, race_status) %>% distinct(), by = "driver_id_name")
    drivers <- drivers %>% mutate(race_status = ifelse(is.na(race_status), "null", race_status))
    drivers$driver_id_name <- NULL
  } else {
    drivers <- drivers %>% mutate(race_status = "null")
  }
} else if (!is.null(drivers)) {
  drivers <- drivers %>% mutate(race_status = "null")
}

# ----------------- Wins by constructor by year computation -----------------
compute_wins <- function(results, races, constructors) {
  if (is.null(results) || is.null(races) || is.null(constructors)) return(NULL)

  # detect winner: positionorder == 1 OR position == "1"
  wins <- results %>%
    mutate(positionorder = ifelse(is.na(positionorder) & "position" %in% names(.),
                                  suppressWarnings(as.integer(position)),
                                  positionorder)) %>%
    filter(!is.na(positionorder) & positionorder == 1)

  # join races (to get year) and constructors (to get name)
  if ("raceid" %in% names(wins) && "raceid" %in% names(races)) {
    wins <- left_join(wins, races %>% select(raceid, year), by = "raceid")
  } else {
    wins$year <- NA_integer_
  }

  if ("constructorid" %in% names(wins) && "constructorid" %in% names(constructors)) {
    wins <- left_join(wins, constructors %>% select(constructorid, name), by = "constructorid")
  } else {
    wins$name <- wins$constructorid
  }

  wins <- wins %>% mutate(year = as.integer(year)) %>%
    group_by(year, constructor = name) %>%
    summarize(wins = n(), .groups = "drop") %>%
    arrange(year, desc(wins))

  # Some years could be NA; remove NA years for plotting
  wins <- wins %>% filter(!is.na(year))
  wins
}

wins_by_year <- compute_wins(results, races, constructors)

# ----------------- UI -----------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* mimic the hand-drawn sketch spacing & look with a subtle clean theme */
      body { background: #fbf6ef; font-family: 'Helvetica', Arial, sans-serif; }
      .panel-left { border: 2px solid #333; border-radius: 6px; padding: 14px; background: #fff; }
      .section-title { font-size: 20px; font-weight: 700; margin-bottom: 8px; }
      .small-note { color: #666; font-size: 12px; }
      .boxed { border: 1px solid #999; padding: 6px; border-radius:4px; background: #fff; }
    "))
  ),

  titlePanel("F1 Fatal Accidents Dashboard", windowTitle = "F1 Fatalities"),

  fluidRow(
    column(width = 3,
           div(class = "panel-left",
               div(class = "section-title", "Controls"),
               sliderInput("year_range", "Slider", min = 1900, max = 2025, value = c(1960, 2020), sep = ""),
               br(),
               uiOutput("country_ui"),
               br(),
               uiOutput("constructor_ui"),
               br(),
               uiOutput("driver_ui"),
               br(),
               radioButtons("race_status","Race Status:",choices=c("All","Fatal","Null"), inline = FALSE)
           )
    ),

    column(width = 6,
           div(class = "boxed", h4("World Map"), leafletOutput("map", height = 420),
               p(class = "small-note", strong("Green = Fatal"), " (red = fatal markers, green = null markers will appear where data exists)"))
    ),

    column(width = 3,
           div(class = "boxed", h4("Wins by Constructor by year"), plotOutput("wins_plot", height = 420))
    )
  ),

  fluidRow(
    column(width = 4,
           div(class = "boxed", h4("Fatalities by cause, by driver age by constructor"), withSpinner(plotOutput("donut_plot", height = 300)))
    ),
    column(width = 4,
           div(class = "boxed", h4("Age by constructor"), withSpinner(plotOutput("age_boxplot", height = 300)))
    ),
    column(width = 4,
           div(class = "boxed", h4("by driver"), uiOutput("driver_info"))
    )
  ),

  br(),
  fluidRow(column(12, p(class = "small-note", "Note: If some plots are empty it means the underlying source CSV did not include the necessary columns (lat/lng for map, incident for donut, DOB for age). The app will still run.")))
)

# ----------------- Server -----------------
server <- function(input, output, session) {

  # UI dynamic inputs
  output$country_ui <- renderUI({
    if (is.null(drivers) || !"nationality" %in% names(drivers)) {
      selectInput("country","Country:", c("All"))
    } else {
      selectInput("country","Country:", c("All", sort(unique(na.omit(drivers$nationality)))))
    }
  })

  output$constructor_ui <- renderUI({
    # prefer constructors$name else fallback to constructors$constructorref or unique from results
    available_cons <- NULL
    if (!is.null(constructors) && "name" %in% names(constructors)) {
      available_cons <- sort(unique(constructors$name))
    } else if (!is.null(results) && "constructorid" %in% names(results)) {
      available_cons <- sort(unique(results$constructorid))
    }
    if (is.null(available_cons)) available_cons <- "All"
    selectInput("constructor","Constructor:", c("All", available_cons))
  })

  output$driver_ui <- renderUI({
    if (is.null(drivers) || !"fullname" %in% names(drivers)) {
      selectInput("driver","Driver:", c("All"))
    } else {
      # show title cased names for selection
      choices <- c("All", sort(unique(drivers$fullname)))
      selectInput("driver","Driver:", choices)
    }
  })

  # reactive filtered drivers (used for map, age, driver info, barplot)
  filtered_drivers <- reactive({
    d <- drivers
    if (is.null(d)) return(NULL)

    # filter by DOB slider if DOB exists
    if ("dob" %in% names(d) && !all(is.na(d$dob))) {
      # try to parse DOB in several common formats
      dob_year <- suppressWarnings(ifelse(is.na(d$dob), NA, tryCatch(year(ymd(d$dob)), error = function(e) NA)))
      if (all(is.na(dob_year))) { # try as ymd not working; try parsing as date with lubridate::parse_date_time
        dob_year <- suppressWarnings(ifelse(is.na(d$dob), NA, year(parse_date_time(d$dob, orders = c("ymd","dmy","mdy")))))
      }
      d$dob_year <- dob_year
      d <- d %>% filter(!is.na(dob_year) & dob_year >= input$year_range[1] & dob_year <= input$year_range[2])
    }

    if (!is.null(input$country) && input$country != "All" && "nationality" %in% names(d)) {
      d <- d %>% filter(nationality == input$country)
    }

    if (!is.null(input$constructor) && input$constructor != "All") {
      # the drivers dataset may not have constructor info; attempt to use results join
      if (!is.null(results) && "driverid" %in% names(results) && "constructorid" %in% names(results)) {
        # find constructor ids matching input constructor name if possible
        if (!is.null(constructors) && "name" %in% names(constructors)) {
          cons_id <- constructors %>% filter(name == input$constructor) %>% pull(constructorid)
          if (length(cons_id)==0) cons_id <- NULL
        } else {
          cons_id <- input$constructor
        }
        if (!is.null(cons_id)) {
          driver_ids <- results %>% filter(constructorid %in% cons_id) %>% pull(driverid) %>% unique()
          if (!is.null(driver_ids) && length(driver_ids)>0 && "driverid" %in% names(d)) {
            d <- d %>% filter(driverid %in% driver_ids)
          }
        }
      }
    }

    if (!is.null(input$driver) && input$driver != "All") {
      d <- d %>% filter(fullname == input$driver)
    }

    if (!is.null(input$race_status) && input$race_status != "All" && "race_status" %in% names(d)) {
      if (input$race_status == "Fatal") d <- d %>% filter(race_status == "fatal")
      if (input$race_status == "Null") d <- d %>% filter(race_status == "null")
    }

    d
  })

  # Map
  output$map <- renderLeaflet({
    d <- filtered_drivers()
    # prefer lat/lng in drivers table; many driver CSVs do not have coordinates.
    has_coords <- !is.null(d) && all(c("lat","lng") %in% names(d))
    if (is.null(d) || nrow(d)==0 || !has_coords) {
      # no coords available — render an empty world map with tiles
      leaflet() %>% addTiles()
    } else {
      leaflet(d) %>% addTiles() %>%
        addCircleMarkers(~lng, ~lat,
                         color = ~ifelse(race_status == "fatal", "red", "green"),
                         radius = 6,
                         stroke = FALSE,
                         fillOpacity = 0.8,
                         popup = ~glue("<b>{fullname}</b><br>Nationality: {nationality}<br>Status: {race_status}"))
    }
  })

  # Wins plot (top-right) — stacked bars of wins by constructor per year; overlay total wins line
  output$wins_plot <- renderPlot({
    wb <- wins_by_year
    if (is.null(wb) || nrow(wb)==0) {
      ggplot() + theme_minimal() + labs(title = "Wins by Constructor by year", subtitle = "Insufficient data (results/races/constructors required)")
    } else {
      # optionally limit to top constructors for visual clarity
      top_cons <- wb %>% group_by(constructor) %>% summarize(total = sum(wins, na.rm=TRUE)) %>% arrange(desc(total)) %>% slice_head(n = 8) %>% pull(constructor)
      wb_small <- wb %>% filter(constructor %in% top_cons)

      # stacked bar by constructor per year
      p <- ggplot(wb_small, aes(x = factor(year), y = wins, fill = constructor)) +
        geom_col(position = "stack") +
        labs(x = "", y = "Wins", caption = "Top constructors shown") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      # overlay total wins per year as a line (computed from all constructors)
      totals <- wb %>% group_by(year) %>% summarize(total_wins = sum(wins, na.rm=TRUE))
      p + geom_line(data = totals, aes(x = factor(year), y = total_wins, group = 1), inherit.aes = FALSE, color = "black", size = 0.6) +
        geom_point(data = totals, aes(x = factor(year), y = total_wins), inherit.aes = FALSE, color = "black", size = 1)
    }
  })

  # Donut (Fatalities by cause or redflags incident)
  output$donut_plot <- renderPlot({
    if (is.null(redflags) || !"incident" %in% names(redflags) || nrow(redflags)==0) {
      # Try to derive causes from drivers_fatal (maybe session/car)
      if (!is.null(drivers_fatal) && any(c("event","session","car") %in% names(drivers_fatal))) {
        dfc <- drivers_fatal %>% mutate(incident = ifelse(!is.na(session), session, "other")) %>% count(incident)
      } else {
        p <- ggplot() + theme_void() + labs(title = "Fatalities by cause (no red_flags available)")
        return(p)
      }
    } else {
      dfc <- redflags %>% count(incident)
    }

    dfc <- dfc %>% mutate(frac = n / sum(n), label = paste0(incident, " (", n, ")"))

    ggplot(dfc, aes(x = 2, y = frac, fill = incident)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      theme_void() +
      theme(legend.position = "right") +
      guides(fill = guide_legend(title = "Cause"))
  })

  # Age by constructor boxplot (center bottom)
  output$age_boxplot <- renderPlot({
    d <- filtered_drivers()
    if (is.null(d) || nrow(d)==0 || !"dob" %in% names(d)) {
      ggplot() + theme_void() + labs(title = "Age by constructor (no DOB available)")
    } else {
      # compute age and try to attach constructor via results if possible
      d <- d %>% mutate(dob_parsed = suppressWarnings(parse_date_time(dob, orders = c("ymd","dmy","mdy"))),
                        age = ifelse(!is.na(dob_parsed), year(Sys.Date()) - year(dob_parsed), NA))
      # if results has driverid and constructorid, join to show constructor grouping
      if (!is.null(results) && "driverid" %in% names(results) && "constructorid" %in% names(results) && "driverid" %in% names(d)) {
        # create small mapping driverid -> constructor names (most frequent)
        driver_ctor <- results %>% filter(!is.na(constructorid)) %>%
          left_join(constructors %>% select(constructorid, name), by = "constructorid") %>%
          group_by(driverid, name) %>% summarize(n = n(), .groups = "drop") %>%
          group_by(driverid) %>% slice_max(n, n = 1) %>% ungroup()
        d2 <- left_join(d, driver_ctor, by = "driverid")
        ggplot(d2, aes(x = name, y = age, fill = name)) +
          geom_boxplot() +
          coord_flip() +
          labs(x = "Constructor", y = "Age")
      } else {
        ggplot(d, aes(x = race_status, y = age, fill = race_status)) +
          geom_boxplot() +
          scale_fill_manual(values = c("fatal" = "red", "null" = "green")) +
          labs(x = "", y = "Age")
      }
    }
  })

  # Driver info box (bottom right)
  output$driver_info <- renderUI({
    d <- filtered_drivers()
    if (is.null(d) || nrow(d)==0 || is.null(input$driver) || input$driver %in% c("", "All")) {
      tagList(
        p("Select a driver to see summary."),
        p("Driver summary will show average age, total races, fatal races, and alive/null races.")
      )
    } else {
      dd <- d
      total <- nrow(dd)
      fatal <- sum(dd$race_status == "fatal", na.rm = TRUE)
      alive <- total - fatal
      avg_age <- NA
      if ("dob" %in% names(dd)) {
        dob_parsed <- suppressWarnings(parse_date_time(dd$dob, orders = c("ymd","dmy","mdy")))
        if (!all(is.na(dob_parsed))) avg_age <- round(mean(year(Sys.Date()) - year(dob_parsed), na.rm = TRUE), 1)
      }
      fluidRow(
        column(12, h4(str_to_title(input$driver))),
        column(12, p(strong("Average Age: "), ifelse(is.na(avg_age), "N/A", avg_age))),
        column(12, p(strong("Total Rows (proxy for races): "), total)),
        column(12, p(strong("Fatal rows: "), fatal)),
        column(12, p(strong("Alive/Null rows: "), alive))
      )
    }
  })
}

# ----------------- Run app -----------------
shinyApp(ui, server)
