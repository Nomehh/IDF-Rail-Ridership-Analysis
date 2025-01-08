library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf)
library(gridExtra)
library(RColorBrewer)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("IDF Railway Dashboard"),
  
  tabsetPanel(
    tabPanel("Carte et graphiques", 
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("referencePeriod", "Sélectionnez la période de référence :", 
                                start = "2018-01-01", end = "2018-12-31"),
                 dateRangeInput("comparisonPeriod", "Sélectionnez la période de comparaison :", 
                                start = "2023-01-01", end = "2023-12-31"),
                 selectInput("station", "Sélectionnez une station :", choices = NULL),
                 checkboxInput("CompareStationToAll", 
                               "Comparaison des statistiques de la station sélectionnée avec la moyenne de toutes les stations", 
                               value = FALSE),
                 plotOutput("holidaysOrNot")
               ),
               mainPanel(
                 leafletOutput("map"),
                 plotOutput("trendPlot"),
                 plotOutput("comparisonPlot")
               )
             )
    ),
    tabPanel("Carte d'affluence animée", 
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("animateMap", "Activer l'animation de la carte", value = TRUE),
                 sliderInput("date_slider", "Sélectionnez la date :", 
                             min = as.Date("2018-01-01"), max = as.Date("2023-12-31"), 
                             value = as.Date("2018-01-01"), timeFormat = "%Y-%m-%d")
               ),
               mainPanel(
                 leafletOutput("map2")
               )
             )
    )
  )
)

vacances_zone_B_C <- data.frame(
  start = as.Date(c(
    "2018-01-01", "2018-02-10", "2018-04-07", "2018-07-07", "2018-10-20", "2018-12-22", # 2018
    "2019-01-01", "2019-02-23", "2019-04-20", "2019-07-06", "2019-10-19", "2019-12-21", # 2019
    "2020-01-01", "2020-02-22", "2020-04-18", "2020-07-04", "2020-10-17", "2020-12-19", # 2020
    "2021-01-01", "2021-02-20", "2021-04-17", "2021-07-03", "2021-10-23", "2021-12-18", # 2021
    "2022-01-01", "2022-02-19", "2022-04-16", "2022-07-02", "2022-10-22", "2022-12-17", # 2022
    "2023-01-01", "2023-02-18", "2023-04-15", "2023-07-01", "2023-10-21", "2023-12-23"  # 2023
  )),
  end = as.Date(c(
    "2018-01-07", "2018-02-25", "2018-04-22", "2018-09-02", "2018-11-04", "2018-12-31", # 2018
    "2019-01-06", "2019-03-10", "2019-05-05", "2019-09-01", "2019-11-03", "2019-12-31", # 2019
    "2020-01-06", "2020-03-08", "2020-05-03", "2020-08-30", "2020-11-01", "2020-12-31", # 2020
    "2021-01-03", "2021-03-07", "2021-05-02", "2021-08-29", "2021-11-07", "2021-12-31", # 2021
    "2022-01-02", "2022-03-06", "2022-05-01", "2022-08-28", "2022-11-06", "2022-12-31", # 2022
    "2023-01-02", "2023-03-05", "2023-04-30", "2023-08-27", "2023-11-05", "2023-12-31"  # 2023
  ))
)

# Fonction pour vérifier si une date est pendant les vacances en zone B ou C
is_vacances_B_C <- function(dates) {
  apply(sapply(dates, function(d) d >= vacances_zone_B_C$start & d <= vacances_zone_B_C$end), 2, any)
}

# Define server logic required to update map & plot based on station selection
server <- function(input, output, session) {
  
  Sys.setlocale("LC_TIME", "English")
  
  # Charger les données
  stations_map <- readRDS("./objects/stations_map.rds")
  stations_map <- st_transform(stations_map, 4326)
  weekly_stats <- readRDS("./objects/weekly_stats.rds")
  month_stats <- readRDS("./objects/month_stats.rds")
  sum_per_lda <- readRDS("./objects/sum_per_lda.rds")
  top_20_lda <- readRDS("./objects/top_20_lda.rds")
  dynamic_map <- readRDS("./objects/dynamic_map.rds")
  
  # Extraire les coordonnées des stations
  coords <- st_coordinates(stations_map)
  stations_map <- stations_map %>%
    mutate(lng = coords[, 1],
           lat = coords[, 2])
  
  updateSelectInput(session, "station",
                    choices = c("Toutes les stations", sort(stations_map$nom)))
  
  day_counter <- reactiveVal(min(dynamic_map$JOUR)) 
  

  # Carte avec toutes les stations
  output$map <- renderLeaflet({
    leaflet(stations_map) %>%
      addTiles() %>%
      addMarkers(
        lng = ~lng,
        lat = ~lat,
        popup = ~nom
      )
  })
  
  # Carte dynamique avec animation des cercles
  output$map2 <- renderLeaflet({
    leaflet(stations_map) %>%
      addTiles() %>%
      setView(lng = 2.3522, lat = 48.8566, zoom = 10) %>%  # Centrer sur Paris
      addMarkers(
        lng = ~lng,
        lat = ~lat,
        popup = ~nom
      )
  })
  
  # Observer pour le choix de la station
  selected_station_data <- reactive({
    req(input$station)
    
    if (input$station == "Toutes les stations") {
      return(stations_map)
    }
    
    filtered_data <- stations_map %>%
      filter(nom == input$station)
    return(filtered_data)
  })
  
  # MàJ de la carte en fonction de la station sélectionnée
  observe({

    avg_val <- sum_per_lda %>%
      filter(nom_lda == input$station) %>%
      summarise(avg_val = mean(total_val)) %>%
        pull() %>%
        round(0)

    selected_station <- selected_station_data()

    if (nrow(selected_station) > 1) {
      text <- selected_station$nom
    } else {
        text <- selected_station$nom %>% as.character() %>% paste(" - Moyenne de voyageur par jour:", avg_val)
    }

    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(
        lng = selected_station$lng,
        lat = selected_station$lat,
        popup = text
      )
  })
  
  # Animation des cercles sur map2
  observe({
    if (input$animateMap) {
      invalidateLater(500, session)  
      
      isolate({  
        current_day <- day_counter()

        s <- dynamic_map %>% filter(JOUR == current_day)
        
        if (nrow(s) <= 0) {
          day_counter(current_day + 1)
          return
        }

        s$normalized_VAL <- (s$VAL - min(s$VAL)) / (max(s$VAL) - min(s$VAL))
        
        pal <- colorQuantile(palette = "Blues", domain = s$normalized_VAL, n = 9)
        
        leafletProxy("map2", data = s) %>%
          clearMarkers() %>%
          addCircleMarkers(
            lng = ~lng,
            lat = ~lat,
            radius = ~normalized_VAL * 10, 
            color = ~pal(normalized_VAL),  
            fillColor = ~pal(normalized_VAL),
            fillOpacity = 0.7,
            popup = ~paste("Valeur:", VAL, "<br>Date:", JOUR),
            stroke = FALSE,
            opacity = 0.8,
            weight = 1
          )
        
          updateSliderInput(session, "date_slider", value = current_day)
        
        if (current_day < max(dynamic_map$JOUR)) {
          day_counter(current_day + 1)
        } else {
          day_counter(min(dynamic_map$JOUR))  
        }
      })
    }
  })
  
  
  # Observer pour le slider et la mise à jour de day_counter
  observeEvent(input$date_slider, {
    day_counter(as.Date(input$date_slider))  
  })
  
  output$trendPlot <- renderPlot({
    selected_station <- selected_station_data()
    if (nrow(selected_station) > 1) {
      title_text <- "Top 20 des stations d'IDF les plus fréquentées"
      
      ggplot(top_20_lda, aes(x = reorder(nom_lda, total_val), y = total_val)) +
        geom_bar(stat = "identity", fill = "#1e90ff", color = "#4682b4") +  
        theme_minimal() +
        labs(title = title_text, x = "Station", y = "Nombre de passagers") +
        theme(
          plot.background = element_rect(fill = "#272b30", color = NA),  
          panel.background = element_rect(fill = "#272b30", color = NA), 
          panel.grid.major = element_line(color = "gray40"),             
          panel.grid.minor = element_line(color = "gray30"),             
          text = element_text(color = "white"),                         
          axis.text = element_text(color = "white"),                   
          axis.title = element_text(color = "white"),                   
          plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5), 
          axis.text.x = element_text(angle = 45, hjust = 1)             
        )
      
    } else {
      if (input$CompareStationToAll) {
        title_text <- paste("Comparaison de la station :", selected_station$nom, "avec la moyenne de toutes les stations")

        selected_station_stats_week <- weekly_stats %>%
          filter(nom_lda == selected_station$nom)

        all_stations_stats_week <- weekly_stats %>%
            group_by(day_of_week) %>%
            summarise(total_val = mean(total_val))

        selected_station_stats_week$day_of_week <- factor(selected_station_stats_week$day_of_week,
                                         levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

        all_stations_stats_week$day_of_week <- factor(all_stations_stats_week$day_of_week,
                                          levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))


        # ajout d'une colonne pour différencier les deux df
        selected_station_stats_week$diff <- "Stations sélectionnée"
        all_stations_stats_week$diff <- "Toutes les stations"

        # concatenation des deux dataframes
        comparison_data <- rbind(selected_station_stats_week, all_stations_stats_week)

        p1 <- ggplot(comparison_data, aes(x = day_of_week, y = total_val, fill = diff)) +
          geom_bar(stat = "identity", position = "dodge") +
          theme_minimal() +
          labs(title = paste(title_text, "par semaine"), x = "Jour de la semaine", y = "Nombre de passagers") +
          theme(
            plot.background = element_rect(fill = "#272b30", color = NA),  
            panel.background = element_rect(fill = "#272b30", color = NA), 
            panel.grid.major = element_line(color = "gray40"),             
            panel.grid.minor = element_line(color = "gray30"),             
            text = element_text(color = "white"),                         
            axis.text = element_text(color = "white"),                   
            axis.title = element_text(color = "white"),                   
            plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5)
          )


        # Month part

        selected_station_stats_month <- month_stats %>%
          filter(nom_lda == selected_station$nom)

        all_stations_stats_month <- month_stats %>%
            group_by(month) %>%
            summarise(total_val = mean(total_val))

        selected_station_stats_month$month <- factor(selected_station_stats_month$month,
                                         levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

        all_stations_stats_month$month <- factor(all_stations_stats_month$month,
                                          levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


        # ajout d'une colonne pour différencier les deux df
        selected_station_stats_month$diff <- "Stations sélectionnée"
        all_stations_stats_month$diff <- "Toutes les stations"

        # concatenation des deux dataframes
        comparison_data_month <- rbind(selected_station_stats_month, all_stations_stats_month)

        p2 <- ggplot(comparison_data_month, aes(x = month, y = total_val, fill = diff)) +
          geom_bar(stat = "identity", position = "dodge") +
          theme_minimal() +
          labs(title = paste(title_text, "par mois"), x = "Mois", y = "Nombre de passagers") +
          theme(
          plot.background = element_rect(fill = "#272b30", color = NA),  
          panel.background = element_rect(fill = "#272b30", color = NA), 
          panel.grid.major = element_line(color = "gray40"),             
          panel.grid.minor = element_line(color = "gray30"),             
          text = element_text(color = "white"),                         
          axis.text = element_text(color = "white"),                   
          axis.title = element_text(color = "white"),                   
          plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5), 
          axis.text.x = element_text(angle = 45, hjust = 1)             
        )


        gridExtra::grid.arrange(p1, p2, ncol = 2)
      } else {
          title_text <- paste("Tendance pour la station :", selected_station$nom)
      # afficher les données weekly_stats pour la station sélectionnée
      selected_station_stats_week <- weekly_stats %>%
        filter(nom_lda == selected_station$nom)

      p1 <- ggplot(selected_station_stats_week, aes(x = day_of_week, y = total_val)) +
        geom_bar(stat = "identity", fill = "#1e90ff", color = "#4682b4") +  
        theme_minimal() +
        labs(title = paste(title_text, "- Hebdomadaire"), x = "Jour de la semaine", y = "Nombre de passagers") + 
        theme(
          plot.background = element_rect(fill = "#272b30", color = NA),  
          panel.background = element_rect(fill = "#272b30", color = NA), 
          panel.grid.major = element_line(color = "gray40"),             
          panel.grid.minor = element_line(color = "gray30"),             
          text = element_text(color = "white"),                         
          axis.text = element_text(color = "white"),                   
          axis.title = element_text(color = "white"),                   
          plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5), 
          axis.text.x = element_text(angle = 45, hjust = 1)             
        )
      

      selected_station_stats_month <- month_stats %>%
        filter(nom_lda == selected_station$nom)

      p2 <- ggplot(selected_station_stats_month, aes(x = month, y = total_val)) +
        geom_bar(stat = "identity", fill = "#1e90ff", color = "#4682b4") +  
        theme_minimal() +
        labs(title = paste(title_text, "- Mensuel"), x = "Mois", y = "Nombre de passagers") + 
        theme(
          plot.background = element_rect(fill = "#272b30", color = NA),  
          panel.background = element_rect(fill = "#272b30", color = NA), 
          panel.grid.major = element_line(color = "gray40"),             
          panel.grid.minor = element_line(color = "gray30"),             
          text = element_text(color = "white"),                         
          axis.text = element_text(color = "white"),                   
          axis.title = element_text(color = "white"),                   
          plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5), 
          axis.text.x = element_text(angle = 45, hjust = 1)             
        )

      gridExtra::grid.arrange(p1, p2, ncol = 2)
      }
    }
  })
  
  output$comparisonPlot <- renderPlot({
    if (nrow(selected_station_data()) > 1) {
      ggplot() +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#272b30", color = NA),  
          panel.background = element_rect(fill = "#272b30", color = NA), 
          text = element_text(color = "white"),
          plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5)
        ) +
        labs(title = "Sélectionnez une station pour comparer", x = "", y = "") 
    } else {
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
      
      # HISTOGRAMME AVEC deux barres pour chaque période (référence et comparaison)
      ggplot(comparison_data, aes(x = day_of_week, y = total_val, fill = period)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_minimal() +
        labs(title = paste("Comparaison entre les périodes de référence et de comparaison pour la station :", selected_station$nom), x = "Jour de la semaine", y = "Nombre de passagers") + 
        theme(
          plot.background = element_rect(fill = "#272b30", color = NA),  
          panel.background = element_rect(fill = "#272b30", color = NA), 
          panel.grid.major = element_line(color = "gray40"),             
          panel.grid.minor = element_line(color = "gray30"),           
          text = element_text(color = "white"),                         
          axis.text = element_text(color = "white"),                  
          axis.title = element_text(color = "white"),                  
          plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5), 
          axis.text.x = element_text(angle = 45, hjust = 1)             
        )
    }
  })

  output$holidaysOrNot <- renderPlot({
    if (nrow(selected_station_data()) > 1) {
      ggplot() +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#272b30", color = NA),  
          panel.background = element_rect(fill = "#272b30", color = NA), 
          text = element_text(color = "white"),
          plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5)
        ) +
        labs(title = "Sélectionnez une station pour afficher l'affluence", x = "", y = "") 
    } else {
      selected_station <- selected_station_data()

      filter_lda <- sum_per_lda %>%
      filter(nom_lda == selected_station$nom)

      # Transformation des données pour inclure les périodes vacances/scolaires
      comparison_data <- filter_lda %>%
      mutate(
        day_of_week = weekdays(JOUR, abbreviate = TRUE),
        period_type = ifelse(is_vacances_B_C(JOUR), "Vacances", "Ouvrable")
      ) %>%
      group_by(day_of_week, period_type) %>%
      summarise(total_val = mean(total_val), .groups = "drop")

      # Ordre des jours de la semaine
      comparison_data$day_of_week <- factor(comparison_data$day_of_week,
        levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

      # Histogramme avec deux barres pour chaque jour de la semaine : vacances vs semaines de cours
      ggplot(comparison_data, aes(x = day_of_week, y = total_val, fill = period_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(
        title = paste("Affluence en semaine de vacances/ouvrable"),
        x = "Jour de la semaine",
        y = "Nombre de passagers",
        fill = "Période"
      ) + 
      theme(
        plot.background = element_rect(fill = "#272b30", color = NA),
        panel.background = element_rect(fill = "#272b30", color = NA),
        panel.grid.major = element_line(color = "gray40"),
        panel.grid.minor = element_line(color = "gray30"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
