library(readr)

val_2018_S1 <- read_delim("data/2018_S1_NB_FER.txt", delim = "\t")
val_2018_S2 <- read_delim("data/2018_S2_NB_Fer.txt", delim = "\t")
val_2023_S1 <- read_delim("data/2023_S1_NB_FER.txt", delim = "\t")

summary(val_2023_S1)

# Rename lda columns of 2023 to match 2018
colnames(val_2023_S1)[colnames(val_2023_S1) == "lda"] <- "ID_REFA_LDA"
summary(val_2023_S1)

# Merge all in one table
validations <- rbind(val_2023_S1, val_2018_S1, val_2018_S2)
summary(validations)