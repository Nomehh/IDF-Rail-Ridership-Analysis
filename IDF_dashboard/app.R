library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf)
library(gridExtra)

ui <- fluidPage(
  titlePanel("IDF Railway Dashboard"),

  sidebarLayout(
    sidebarPanel(
      dateRangeInput("referencePeriod", "Sélectionnez la période de référence :",
                     start = Sys.Date() - 60, end = Sys.Date() - 30),
      dateRangeInput("comparisonPeriod", "Sélectionnez la période de comparaison :",
                     start = Sys.Date() - 30, end = Sys.Date()),
      selectInput("station", "Sélectionnez une station :", choices = NULL)
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput("trendPlot"),
      plotOutput("comparisonPlot")
    )
  )
)

## Partie un peu crade ##
# Charger les données
stations_map <- readRDS("../stations_map.rds")
stations_map <- st_transform(stations_map, 4326)
weekly_stats <- readRDS("../weekly_stats.rds")
month_stats <- readRDS("../month_stats.rds")
sum_per_lda <- readRDS("../sum_per_lda.rds")

# Extraire les coordonnées des stations
coords <- st_coordinates(stations_map)
stations_map <- stations_map %>%
  mutate(lng = coords[, 1],
         lat = coords[, 2])

# Define server logic required to update map & plot based on station selection
server <- function(input, output, session) {

  updateSelectInput(session, "station",
                    choices = c("Toutes les stations", stations_map$nom))

  print(colnames(weekly_stats))

  # Carte
  output$map <- renderLeaflet({
    leaflet(stations_map) %>%
      addTiles() %>%
      addMarkers(
        lng = ~lng,
        lat = ~lat,
        popup = ~nom
      )
  })

  # Observer qui update la carte en fct du choix
  selected_station_data <- reactive({
    req(input$station)

    if (input$station == "Toutes les stations") {
      return(stations_map)
    }

    filtered_data <- stations_map %>%
      filter(nom == input$station)
    return(filtered_data)
  })

  # MàJ de la carte
  observe({
    selected_station <- selected_station_data()

    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(
        lng = selected_station$lng,
        lat = selected_station$lat,
        popup = selected_station$nom
      )
  })

  output$trendPlot <- renderPlot({
    selected_station <- selected_station_data()
    if (nrow(selected_station) > 1) {
      title_text <- "Tendance pour toutes les stations"

      # afficher les données weekly_stats pour toutes les stations
      ggplot(selected_station, aes(x = nom, y = avg_val)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = title_text)
    } else {

      title_text <- paste("Tendance pour la station :", selected_station$nom)
      # afficher les données weekly_stats pour la station sélectionnée
      selected_station_stats_week <- weekly_stats %>%
        filter(nom_lda == selected_station$nom)

      p1 <- ggplot(selected_station_stats_week, aes(x = day_of_week, y = total_val)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = paste(title_text, "- Hebdomadaire"), x = "Jour de la semaine", y = "Nombre de passagers")

      selected_station_stats_month <- month_stats %>%
        filter(nom_lda == selected_station$nom)

      p2 <- ggplot(selected_station_stats_month, aes(x = month, y = total_val)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = paste(title_text, "- Mensuel"), x = "Mois", y = "Nombre de passagers")

      gridExtra::grid.arrange(p1, p2, ncol = 2)
    }
  })

  output$comparisonPlot <- renderPlot({
    if (nrow(selected_station_data()) > 1) {
      return()
    }
    selected_station <- selected_station_data()

    filter_lda <- sum_per_lda %>%
      filter(nom_lda == selected_station$nom)

    reference_data <- filter_lda %>%
        filter(JOUR >= as.Date(input$referencePeriod[1]) & JOUR <= as.Date(input$referencePeriod[2]))

    comparison_data <- filter_lda %>%
      filter(JOUR >= as.Date(input$comparisonPeriod[1]) & JOUR <= as.Date(input$comparisonPeriod[2]))

    reference_data <- reference_data %>%
      mutate(day_of_week = weekdays(JOUR, abbreviate = TRUE)) %>%
      group_by(day_of_week) %>%
      summarise(total_val = mean(total_val))

    comparison_data <- comparison_data %>%
        mutate(day_of_week = weekdays(JOUR, abbreviate = TRUE)) %>%
        group_by(day_of_week) %>%
        summarise(total_val = mean(total_val))

    reference_data$day_of_week <- factor(reference_data$day_of_week,
      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

    comparison_data$day_of_week <- factor(comparison_data$day_of_week,
        levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

    # ajout d'une colonne pour différencier les deux périodes
    reference_data$period <- input$referencePeriod[1] %>% as.character() %>% paste(" - ", input$referencePeriod[2])
    comparison_data$period <- input$comparisonPeriod[1] %>% as.character() %>% paste(" - ", input$comparisonPeriod[2])

    # concatenation des deux dataframes
    comparison_data <- rbind(reference_data, comparison_data)

    # HISTOGRAMME AVEC deux bar pour chaque données (reference et comparison)
    ggplot(comparison_data, aes(x = day_of_week, y = total_val, fill = period)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = paste("Comparaison entre les périodes de référence et de comparaison pour la station :", selected_station$nom), x = "Jour de la semaine", y = "Nombre de passagers")

  })
}

# Run the application
shinyApp(ui = ui, server = server)
