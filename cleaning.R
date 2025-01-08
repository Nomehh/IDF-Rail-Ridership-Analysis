library(readr)
library(sf)
library(ggplot2)
library(dplyr)
library(osmdata)
library(tidyverse)
library(beepr)
library(lubridate)

Sys.setlocale("LC_TIME", "English")

# Load data
val_2018_S1 <- read_delim("data/2018_S1_NB_FER.txt", delim = "\t")
val_2018_S2 <- read_delim("data/2018_S2_NB_Fer.txt", delim = "\t")
val_2019_S1 <- read_delim("data/2019_S1_NB_FER.txt", delim = "\t")
val_2019_S2 <- read_delim("data/2019_S2_NB_FER.txt", delim = "\t")
val_2020_S1 <- read_delim("data/2020_S1_NB_FER.txt", delim = "\t")
val_2020_S2 <- read_delim("data/2020_S2_NB_FER.txt", delim = "\t")
val_2021_S1 <- read_delim("data/2021_S1_NB_FER.txt", delim = "\t")
val_2021_S2 <- read_delim("data/2021_S2_NB_FER.txt", delim = "\t")
val_2022_S1 <- read_delim("data/2022_S1_NB_FER.txt", delim = "\t")
val_2022_S2 <- read_delim("data/2022_S2_NB_FER.txt", delim = ";")
val_2023_S1 <- read_delim("data/2023_S1_NB_FER.txt", delim = "\t")
val_2023_S2 <- read_delim("data/2023_S2_NB_FER.txt", delim = "\t")

geo_data <- sf::st_read("data/geo/PL_ZDL_R_13_12_2024.shp")

# Rename lda columns of 2022/2023 to match 2018
colnames(val_2022_S2)[colnames(val_2022_S2) == "lda"] <- "ID_REFA_LDA"
colnames(val_2023_S1)[colnames(val_2023_S1) == "lda"] <- "ID_REFA_LDA"
colnames(val_2023_S2)[colnames(val_2023_S2) == "ID_ZDC"] <- "ID_REFA_LDA"

# Merge all in one table
validations <- rbind(val_2018_S1, val_2018_S2, val_2019_S1, val_2019_S2, val_2020_S1, val_2020_S2, val_2021_S1, val_2021_S2, val_2022_S1, val_2022_S2, val_2023_S1, val_2023_S2)

# Convertir la colonne JOUR en type Date si ce n'est pas déjà fait
validations$JOUR <- as.Date(validations$JOUR, format = "%d/%m/%Y")

# Filtrer en retirant les arrets de bus et tram
geo_data <- geo_data %>%
    filter(type_arret != "Arrêt de bus" & type_arret != "Arrêt de tram")

# Garder les noms lda + leur id pour les validations (sans la géométrie)
nom_arret <- geo_data %>%
  st_drop_geometry() %>%
  group_by(idrefa_lda) %>%
  summarise(nom_lda = first(nom_lda)) %>%
  select(idrefa_lda, nom_lda)

# total de validations par LDA avec localisation des LDA
total_val_lda <- validations %>%
  group_by(ID_REFA_LDA) %>%
  summarise(total_val = sum(NB_VALD)) %>%
  left_join(nom_arret, by = c("ID_REFA_LDA" = "idrefa_lda"))

# supprimer les LDA inconnus (NA)
total_val_lda <- total_val_lda %>%
  filter(nom_lda != 'NA')

# Top 20 des stations les plus fréquentées
top_20_lda <- total_val_lda %>%
  filter(ID_REFA_LDA != 0) %>%
  arrange(desc(total_val)) %>%
  head(20)

# Sauvegarde
saveRDS(top_20_lda, "top_20_lda.rds")

# Agréger les géométries des arrêts par LDA et calculer le centroïde (LONG SA MERE)
centroid_lda <- geo_data %>%
  group_by(idrefa_lda) %>%
  summarise(geometry = st_union(geometry)) %>% # Necessaire pour avoir un seul point par LDA
  st_centroid()

# map of avg validation per day per LDA
day_avg_val_lda <- validations %>%
  group_by(ID_REFA_LDA, JOUR) %>%
  summarise(total_val = sum(NB_VALD),.groups = "drop") %>% # TOTAL de validation par LDA par jour (car LDA = plusieurs arrêts)
  group_by(ID_REFA_LDA) %>%
  summarise(avg_val = mean(total_val),.groups = "drop") # Moyenne de validation par LDA

map <- day_avg_val_lda %>%
  left_join(centroid_lda, by = c("ID_REFA_LDA" = "idrefa_lda")) %>%
  left_join(nom_arret, by = c("ID_REFA_LDA" = "idrefa_lda")) %>%
  st_as_sf()

# renommer la colonne nom_lda en nom
colnames(map)[colnames(map) == "nom_lda"] <- "nom"

# sauvegarder la map
stations_map <- map %>%
  select(avg_val, nom, geometry, ID_REFA_LDA) %>%
  na.omit() %>%
  st_as_sf()
saveRDS(stations_map, "stations_map.rds")

# Fréquetation des stations par jour de la semaine
weekly_stats <- validations %>%
  left_join(nom_arret, by = c("ID_REFA_LDA" = "idrefa_lda")) %>%
  mutate(day_of_week = weekdays(JOUR, abbreviate = TRUE)) %>% # Réccupérer le jour de la semaine en fonction de la date
  group_by(nom_lda, day_of_week) %>%
  summarise(total_val = mean(NB_VALD),.groups = "drop")

# Réordonner les jours de la semaine
weekly_stats$day_of_week <- factor(weekly_stats$day_of_week,
  levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
)

# Selectionner les colonnes importantes
weekly_stats <- weekly_stats %>%
  select(nom_lda, day_of_week, total_val)

# Sauvegarder
saveRDS(weekly_stats, "weekly_stats.rds")

# Frequentation des stations par mois
month_stats <- validations %>%
  left_join(nom_arret, by = c("ID_REFA_LDA" = "idrefa_lda")) %>%
  mutate(month = months(JOUR, abbreviate = TRUE)) %>% # Récupérer le mois en fonction de la date
  group_by(nom_lda, month) %>%
  summarise(total_val = mean(NB_VALD), .groups = "drop")

# Réordonner les mois
month_stats$month <- factor(month_stats$month,
  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)

# Selectionner les colonnes importantes
month_stats <- month_stats %>%
  select(nom_lda, month, total_val)

# Sauvegarder
saveRDS(month_stats, "month_stats.rds")

# Frequentation par stations pour les ensuite filtrer sur l'app
Sum_per_lda <- validations %>%
  left_join(nom_arret, by = c("ID_REFA_LDA" = "idrefa_lda")) %>%
  group_by(nom_lda, JOUR) %>%
  summarise(total_val = mean(NB_VALD), .groups = "drop")

Sum_per_lda <- Sum_per_lda %>%
  select(nom_lda, JOUR, total_val)

saveRDS(Sum_per_lda, "sum_per_lda.rds")

# Map dynamique qui affiche les validations par LDA par jour
map_val_lda <- validations %>%
  group_by(JOUR, ID_REFA_LDA) %>%
  summarise(VAL = sum(NB_VALD), .groups = "drop") %>%
  na.omit() %>% # Retirer les LDA inconnus
  filter(ID_REFA_LDA != 0) %>%
  merge(stations_map) %>%
  st_as_sf() %>%
  st_transform(4326) %>%
  mutate(coord = st_coordinates(.)) %>%
  mutate(lng = coord[, 1], lat = coord[, 2]) %>%
  select(-coord)

saveRDS(map_val_lda, "dynamic_map.rds")



### Générer les graphes du rapport

# Graphe chronologique par mois
monthly_ridership <- monthly_ridership %>%
  mutate(year_month = paste(month, year, sep = "_"))
saveRDS(monthly_ridership, "doc_monthly_ridership.rds")

# Comparaison saisonnière entre Lagny et Disney
stations_of_interest <- c("Lagny - Thorigny", "Marne-la-Vallée Chessy")

seasonal_comparison <- month_stats %>%
  filter(nom_lda %in% stations_of_interest)
saveRDS(seasonal_comparison, "doc_seasonal_comparison.rds")

# Fréquentation des stations en semaine
stations_of_interest <- c("La Défense", "Marne-la-Vallée Chessy")

weekly_frequentation <- weekly_stats %>%
  filter(nom_lda %in% stations_of_interest)
saveRDS(weekly_frequentation, "doc_weekly_frequentation.rds")
