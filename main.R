library(readr)
library(sf)
library(ggplot2)
library(dplyr)

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

nom_arret <- geo_data %>% st_drop_geometry() %>%
  group_by(idrefa_lda) %>%
  summarise(nom_lda = first(nom_lda)) %>%
  select(idrefa_lda, nom_lda)

# Agréger les géométries des arrêts par LDA et calculer le centroïde (LONG SA MERE)
centroid_lda <- geo_data %>%
  group_by(idrefa_lda) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_centroid()


total_val_lda <- validations %>% group_by(ID_REFA_LDA) %>%
  summarise(total_val = sum(NB_VALD)) %>%
  left_join(nom_arret, by = c("ID_REFA_LDA" = "idrefa_lda"))

# TOP 20 of LDA
total_val_lda %>% filter(ID_REFA_LDA != 0) %>%
  arrange(desc(total_val)) %>%
  head(20) %>%
  ggplot(aes(x = reorder(nom_lda, total_val), y = total_val)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Validation par LDA",
       x = "Nom LDA",
       y = "Nombre de validations")


# TODO : MAP -> ajouter un fond !!
# map of avg validation per day per LDA
day_avg_val_lda <- validations %>% group_by(ID_REFA_LDA, JOUR) %>%
  summarise(total_val = sum(NB_VALD)) %>% # TOTAL de validation par LDA par jour (car LDA = plusieurs arrêts)
  group_by(ID_REFA_LDA) %>%
  summarise(avg_val = mean(total_val)) # Moyenne de validation par LDA

map = day_avg_val_lda %>%
  left_join(centroid_lda, by = c("ID_REFA_LDA" = "idrefa_lda")) %>%
  left_join(nom_arret, by = c("ID_REFA_LDA" = "idrefa_lda")) %>%
  st_as_sf()

ggplot(map)+geom_sf(aes(size=avg_val),color="red")+scale_size_area(max_size = 4)

# zoom on Paris
map_name_top = map %>% filter(ID_REFA_LDA != 0) %>%
  arrange(desc(avg_val)) %>%
  head(20)

ggplot(map)+
  geom_sf(aes(size=avg_val),color="red")+
  scale_size_area(max_size = 4)+
  coord_sf(xlim = c(645000, 657500), ylim = c(6857800, 6867200))+
  geom_sf_text(data = map_name_top, aes(label = nom_lda), size = 3, nudge_y = 0.01) # TODO: Position du text a ajuster