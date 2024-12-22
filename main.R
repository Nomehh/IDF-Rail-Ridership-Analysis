library(readr)
library(sf)
library(ggplot2)
library(dplyr)

Sys.setlocale("LC_TIME", "English")

val_2018_S1 <- read_delim("data/2018_S1_NB_FER.txt", delim = "\t")
val_2018_S2 <- read_delim("data/2018_S2_NB_Fer.txt", delim = "\t")
val_2023_S1 <- read_delim("data/2023_S1_NB_FER.txt", delim = "\t")

geo_data <- sf::st_read("data/geo/PL_ZDL_R_13_12_2024.shp")

summary(val_2023_S1)

# Rename lda columns of 2023 to match 2018
colnames(val_2023_S1)[colnames(val_2023_S1) == "lda"] <- "ID_REFA_LDA"
summary(val_2023_S1)

# Merge all in one table
validations <- rbind(val_2023_S1, val_2018_S1, val_2018_S2)
summary(validations)

# Convertir la colonne JOUR en type Date si ce n'est pas déjà fait
validations$JOUR <- as.Date(validations$JOUR, format = "%d/%m/%Y")

nom_arret <- geo_data %>%
  st_drop_geometry() %>%
  group_by(idrefa_lda) %>%
  summarise(nom_lda = first(nom_lda)) %>%
  select(idrefa_lda, nom_lda)

total_val_lda <- validations %>%
  group_by(ID_REFA_LDA) %>%
  summarise(total_val = sum(NB_VALD)) %>%
  left_join(nom_arret, by = c("ID_REFA_LDA" = "idrefa_lda"))


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

## MAP
# Agréger les géométries des arrêts par LDA et calculer le centroïde (LONG SA MERE)
centroid_lda <- geo_data %>%
  group_by(idrefa_lda) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_centroid()

# TODO : MAP -> ajouter un fond !!
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

ggplot(map) +
  geom_sf(aes(size = avg_val), color = "red") +
  scale_size_area(max_size = 4)

# zoom on Paris
map_name_top <- map %>%
  filter(ID_REFA_LDA != 0) %>%
  arrange(desc(avg_val)) %>%
  head(20)

ggplot(map) +
  geom_sf(aes(size = avg_val), color = "red") +
  scale_size_area(max_size = 4) +
  coord_sf(xlim = c(645000, 657500), ylim = c(6857800, 6867200)) +
  geom_sf_text(data = map_name_top, aes(label = nom_lda), size = 3, nudge_y = 0.01) # TODO: Position du text a ajuster

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
# Grosse baisse de validation en aout mais de Septembre a Decembre ont ne retrouve pas les valeurs de Janvier a Juillet ??
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
is_vacances_B_C <- function(date) {
  any(sapply(1:nrow(vacances_zone_B_C), function(i) date >= vacances_zone_B_C$start[i] & date <= vacances_zone_B_C$end[i]))
}


validations_ile_de_france <- validations %>% # Possiblement très long...
  mutate(
    period = ifelse(sapply(JOUR, is_vacances_B_C), "Vacances", "Normal"),
    month = months(JOUR, abbreviate = TRUE)  # Mois abrégé
  )


validation_per_month_ile_de_france <- validations_ile_de_france %>%
  group_by(month, period) %>%
  summarise(total_val = sum(NB_VALD)) %>%
  ungroup()

validation_per_month_ile_de_france$month <- factor(validation_per_month_ile_de_france$month,
                                                   levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

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



