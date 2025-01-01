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
      dateRangeInput("dateRange", "Sélectionnez une période :", 
                     start = Sys.Date() - 30, end = Sys.Date()),
      selectInput("station", "Sélectionnez une station :", choices = NULL)
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput("trendPlot"),
    )
  )
)

## Partie un peu crade ##
# Charger les données
stations_map <- readRDS("../stations_map.rds")
stations_map <- st_transform(stations_map, 4326)
weekly_stats <- readRDS("../weekly_stats.rds")
month_stats <- readRDS("../month_stats.rds")


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
        labs(title = paste(title_text, "- Hebdomadaire"))

      selected_station_stats_month <- month_stats %>%
        filter(nom_lda == selected_station$nom)

      p2 <- ggplot(selected_station_stats_month, aes(x = month, y = total_val)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = paste(title_text, "- Mensuel"))

      gridExtra::grid.arrange(p1, p2, ncol = 2)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
