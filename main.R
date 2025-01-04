library(readr)
library(sf)
library(ggplot2)
library(dplyr)
library(osmdata)
library(tidyverse)
library(beepr)

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

summary(val_2023_S1)

# Rename lda columns of 2023 to match 2018
colnames(val_2022_S2)[colnames(val_2022_S2) == "lda"] <- "ID_REFA_LDA"
colnames(val_2023_S1)[colnames(val_2023_S1) == "lda"] <- "ID_REFA_LDA"
colnames(val_2023_S2)[colnames(val_2023_S2) == "ID_ZDC"] <- "ID_REFA_LDA"

summary(val_2023_S1)

val_2023_S2$CATEGORIE_TITRE[val_2023_S2$CATEGORIE_TITRE == "Contrat Solidarit\x8e Transport"] <- "FGT"
val_2023_S2$CATEGORIE_TITRE[val_2023_S2$CATEGORIE_TITRE == "Autres titres"] <- "AUTRE TITRE"
val_2023_S2$CATEGORIE_TITRE[val_2023_S2$CATEGORIE_TITRE == "Amethyste"] <- "AMETHYSTE"
val_2023_S2$CATEGORIE_TITRE[val_2023_S2$CATEGORIE_TITRE == "Imagine R"] <- "IMAGINE R"
val_2023_S2$CATEGORIE_TITRE[val_2023_S2$CATEGORIE_TITRE == "Forfaits courts"] <- "NAVIGO JOUR"
val_2023_S2$CATEGORIE_TITRE[val_2023_S2$CATEGORIE_TITRE == "Forfait Navigo"] <- "NAVIGO"

# Merge all in one table
validations <- rbind(val_2018_S1, val_2018_S2, val_2019_S1, val_2019_S2, val_2020_S1, val_2020_S2, val_2021_S1, val_2021_S2, val_2022_S1, val_2022_S2, val_2023_S1, val_2023_S2)

validations$CATEGORIE_TITRE[validations$CATEGORIE_TITRE == "?"] <- "AUTRE TITRE"
validations$CATEGORIE_TITRE[validations$CATEGORIE_TITRE == "NON DEFINI"] <- "AUTRE TITRE"

summary(validations)

# Convertir la colonne JOUR en type Date si ce n'est pas déjà fait
validations$JOUR <- as.Date(validations$JOUR, format = "%d/%m/%Y")

# Filtrer en retirant les arrets de bus et tram
geo_data <- geo_data %>%
    filter(type_arret != "Arrêt de bus" & type_arret != "Arrêt de tram")

nom_arret <- geo_data %>%
  st_drop_geometry() %>%
  group_by(idrefa_lda) %>%
  summarise(nom_lda = first(nom_lda)) %>%
  select(idrefa_lda, nom_lda)

total_val_lda <- validations %>%
  group_by(ID_REFA_LDA) %>%
  summarise(total_val = sum(NB_VALD)) %>%
  left_join(nom_arret, by = c("ID_REFA_LDA" = "idrefa_lda"))

# supprimer les LDA inconnus (NA)
total_val_lda <- total_val_lda %>%
  filter(nom_lda != 'NA')

## STAT stations

# TOP 20 of LDA
total_val_lda %>%
  filter(ID_REFA_LDA != 0) %>%
  arrange(desc(total_val)) %>%
  head(20) %>%
  ggplot(aes(x = reorder(nom_lda, total_val), y = total_val)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Validation par LDA",
    x = "Nom LDA",
    y = "Nombre de validations"
  )

top_20_lda <- total_val_lda %>%
  filter(ID_REFA_LDA != 0) %>%
  arrange(desc(total_val)) %>%
  head(20)

saveRDS(top_20_lda, "top_20_lda.rds")


ggplot(top_20_lda, aes(x = reorder(nom_lda, total_val), y = total_val)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top 20", x = "Station", y = "Nombre de passagers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## MAP
# Agréger les géométries des arrêts par LDA et calculer le centroïde (LONG SA MERE)
centroid_lda <- geo_data %>%
  group_by(idrefa_lda) %>%
  summarise(geometry = st_union(geometry)) %>% # Necessaire pour avoir un seul point par LDA
  st_centroid()

unique(geo_data$type_arret)

# map of avg validation per day per LDA
day_avg_val_lda <- validations %>%
  group_by(ID_REFA_LDA, JOUR) %>%
  summarise(total_val = sum(NB_VALD)) %>% # TOTAL de validation par LDA par jour (car LDA = plusieurs arrêts)
  group_by(ID_REFA_LDA) %>%
  summarise(avg_val = mean(total_val)) # Moyenne de validation par LDA

map <- day_avg_val_lda %>%
  left_join(centroid_lda, by = c("ID_REFA_LDA" = "idrefa_lda")) %>%
  left_join(nom_arret, by = c("ID_REFA_LDA" = "idrefa_lda")) %>%
  st_as_sf()


# renommer la colonne nom_lda en nom
colnames(map)[colnames(map) == "nom_lda"] <- "nom"

# sauvegarder la map
stations_map <- map %>%
  select(avg_val, nom, geometry) %>%
  na.omit() %>%
  st_as_sf()
saveRDS(stations_map, "stations_map.rds")

# Fréquetation des stations par jour de la semaine
weekly_stats <- validations %>%
  left_join(nom_arret, by = c("ID_REFA_LDA" = "idrefa_lda")) %>%
  mutate(day_of_week = weekdays(JOUR, abbreviate = TRUE)) %>%
  group_by(nom_lda, day_of_week) %>%
  summarise(total_val = mean(NB_VALD))

weekly_stats$day_of_week <- factor(weekly_stats$day_of_week,
  levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
)

weekly_stats <- weekly_stats %>%
  select(nom_lda, day_of_week, total_val)

saveRDS(weekly_stats, "weekly_stats.rds")

# Frequentation des stations par mois
month_stats <- validations %>%
  left_join(nom_arret, by = c("ID_REFA_LDA" = "idrefa_lda")) %>%
  mutate(month = months(JOUR, abbreviate = TRUE)) %>%
  group_by(nom_lda, month) %>%
  summarise(total_val = mean(NB_VALD))

month_stats$month <- factor(month_stats$month,
  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)

month_stats <- month_stats %>%
  select(nom_lda, month, total_val)

saveRDS(month_stats, "month_stats.rds")

# Frequentation par stations pour les ensuite filtrer sur l'app
Sum_per_lda <- validations %>%
  left_join(nom_arret, by = c("ID_REFA_LDA" = "idrefa_lda")) %>%
  group_by(nom_lda, JOUR) %>%
  summarise(total_val = mean(NB_VALD))

Sum_per_lda <- Sum_per_lda %>%
  select(nom_lda, JOUR, total_val)



saveRDS(Sum_per_lda, "sum_per_lda.rds")


# zoom on Paris
map_name_top <- map %>%
  filter(ID_REFA_LDA != 0) %>%
  arrange(desc(avg_val)) %>%
  head(20)

# ville cible
area <- "Paris, France"

# récupérer les délimitation de la ville
streets <- getbb(area) %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary",
                            "secondary", "tertiary")) %>%
  osmdata_sf()

# Récupérer la Seine
river <- getbb(area) %>%
  opq() %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

# récupérer les petites rues
small_streets <- getbb(area) %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "pedestrian", "service")) %>%
  osmdata_sf()

# Affichage de chaque élements de la carte de Paris + les LDA
(p <- ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "grey",
          size = .4,
          alpha = .6) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "91939e",
          size = .2,
          alpha = .4) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "#0062FF",
          size = .3,
          alpha = .8) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#282828")) +
  geom_sf(data = map, aes(size = avg_val), color = "red") +
  scale_size_area(max_size = 4) +
  coord_sf(xlim = c(2.224, 2.469), ylim = c(48.815, 48.902)) +
geom_sf_text(data = map_name_top, aes(label = nom), size = 2.5, nudge_y = -0.003, color = "snow"))

## STAT temps

# histogram over year (pas intéressant)
validations %>%
  group_by(JOUR) %>%
  summarise(total_val = sum(NB_VALD)) %>%
  ggplot(aes(x = JOUR, y = total_val)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Validation par jour",
    x = "Jour",
    y = "Nombre de validations"
  )

# histogram validation moyenne par jour de la semaine
# colonne jour de la semaine
week_val <- validations %>%
  mutate(day_of_week = weekdays(JOUR, abbreviate = TRUE))

# moyenne par jour de la semaine
avg_validation_per_day <- week_val %>%
  group_by(day_of_week) %>%
  summarise(avg_val = mean(NB_VALD))

avg_validation_per_day$day_of_week <- factor(avg_validation_per_day$day_of_week,
  levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
)

ggplot(avg_validation_per_day, aes(x = day_of_week, y = avg_val)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Validation moyenne par jour de la semaine",
    x = "Jour de la semaine",
    y = "Nombre moyen de validations"
  )

# On vois qu'a disney la chutte du weekend est moins importante
disney <- validations %>%
  filter(ID_REFA_LDA == 68385) %>%
  mutate(day_of_week = weekdays(JOUR, abbreviate = TRUE)) %>%
  group_by(day_of_week) %>%
  summarise(avg_val = mean(NB_VALD))

disney$day_of_week <- factor(disney$day_of_week,
  levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
)

ggplot(disney, aes(x = day_of_week, y = avg_val)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Validation moyenne par jour de la semaine a Disney",
    x = "Jour de la semaine",
    y = "Nombre moyen de validations"
  )

# moyenne par mois
month_val <- validations %>%
  mutate(month = months(JOUR, abbreviate = TRUE))

avg_validation_per_month <- month_val %>%
  group_by(month) %>%
  summarise(avg_val = mean(NB_VALD))

avg_validation_per_month$month <- factor(avg_validation_per_month$month,
  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)

ggplot(avg_validation_per_month, aes(x = month, y = avg_val)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Validation moyenne par mois",
    x = "Mois",
    y = "Nombre moyen de validations"
  )


# Différence du nombre de validation en fonction du type des titres
validations %>%
  group_by(CATEGORIE_TITRE) %>%
  summarise(total_val = sum(NB_VALD)) %>%
  ggplot(aes(x = reorder(CATEGORIE_TITRE, total_val), y = total_val)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Validation par type de titre",
    x = "Type de titre",
    y = "Nombre de validations"
  )

# Différence du nombre de validation en fonction du type des titres par mois -> pas très intéressant
# TST -> Tarification Solidarité Transport
# FGT -> Forfait Gratuité Transport
# NAVIGO -> Forfait Navigo (annuel, mensuel, semaine)
# IMAGINE R -> Forfait Imagine R (etudiant + scolaire)
# Amethyste -> Forfait Amethyste (personne agée)
# Navigo jour -> Forfait Navigo jour
# AUTRE + ? + NON DEFINI -> Autre

validations %>%
  group_by(CATEGORIE_TITRE) %>%
  summarise(total_val = sum(NB_VALD))

validation_type_per_month <- validations %>%
  mutate(month = months(JOUR, abbreviate = TRUE)) %>%
  group_by(CATEGORIE_TITRE, month) %>%
  summarise(total_val = sum(NB_VALD))

validation_type_per_month$month <- factor(validation_type_per_month$month,
  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)

validation_type_per_month %>% ggplot(aes(x = month, y = total_val, fill = CATEGORIE_TITRE)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Validation par type de titre par mois",
    x = "Mois",
    y = "Nombre de validations",
    fill = "Type de titre"
  )

# Différence du nombre de validation en fonction du type des titres par jour de la semaine -> pas très intéressant
# Les validations sont plus importantes en semaine (normal) mais on ne voit pas de différence entre les types de titre
validation_type_per_day <- validations %>%
  mutate(day_of_week = weekdays(JOUR, abbreviate = TRUE)) %>%
  group_by(CATEGORIE_TITRE, day_of_week) %>%
  summarise(total_val = sum(NB_VALD))

validation_type_per_day$day_of_week <- factor(validation_type_per_day$day_of_week,
  levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
)

validation_type_per_day %>% ggplot(aes(x = day_of_week, y = total_val, fill = CATEGORIE_TITRE)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Validation par type de titre par jour de la semaine",
    x = "Jour de la semaine",
    y = "Nombre de validations",
    fill = "Type de titre"
  )


library(lubridate)
vacances_zone_B_C <- data.frame(
  start = as.Date(c("2018-10-20", "2018-12-22", "2018-02-10", "2018-04-07", "2018-07-07")),
  end = as.Date(c("2018-11-04", "2018-12-31", "2018-02-25", "2018-04-22", "2018-09-03"))
)

# Fonction pour vérifier si une date est pendant les vacances en zone B ou C
is_vacances_B_C <- function(dates) {
  apply(sapply(dates, function(d) d >= vacances_zone_B_C$start & d <= vacances_zone_B_C$end), 2, any)
}

validations_ile_de_france <- validations %>%
  mutate(
    period = ifelse(is_vacances_B_C(JOUR), "Vacances", "Normal"),
    month = months(JOUR, abbreviate = TRUE)
  )

validation_per_month_ile_de_france <- validations_ile_de_france %>%
  group_by(month, period) %>%
  summarise(total_val = sum(NB_VALD)) %>%
  ungroup()

validation_per_month_ile_de_france$month <- factor(validation_per_month_ile_de_france$month,
  levels = c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
)

validation_per_month_ile_de_france %>%
  ggplot(aes(x = month, y = total_val, fill = period)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(
    title = "Validation par mois et par période scolaire (Île-de-France - 2018)",
    x = "Mois",
    y = "Nombre de validations",
    fill = "Période"
  )
