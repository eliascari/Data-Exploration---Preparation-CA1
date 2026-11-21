library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(scales)

population <- read.csv(
  file.choose(),
  stringsAsFactors = FALSE
)

dim(population)

names(population)

head(population)

summary(population)

#checking for missing values
missing_values <- colSums(is.na(population))

missing_values

#checking for duplicates
sum(duplicated(population))

#data types
str(population)

#replacing missing values with NA
population_clean <- population %>% mutate(
  Iso2 = ifelse(
    is.na(Iso2),
    "NA",
    Iso2
  )
)

sum(is.na(population_clean$Iso2))