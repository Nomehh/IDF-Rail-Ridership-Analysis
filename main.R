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

val_lda <- validations %>% group_by(ID_REFA_LDA) %>%
  summarise(total_val = sum(NB_VALD)) %>%
  left_join(nom_arret, by = c("ID_REFA_LDA" = "idrefa_lda"))

val_lda %>% filter(ID_REFA_LDA != 0) %>%
  arrange(desc(total_val)) %>%
  head(20) %>%
  ggplot(aes(x = reorder(nom_lda, total_val), y = total_val)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Validation par LDA",
       x = "Nom LDA",
       y = "Nombre de validations")

# map of total validations per LDA (no sense)
map =  val_lda %>% left_join(geo_data,by = c("ID_REFA_LDA" = "idrefa_lda")) %>% st_as_sf() %>% st_centroid()
ggplot(map)+geom_sf(aes(size=total_val),color="red")+scale_size_area(max_size = 4)