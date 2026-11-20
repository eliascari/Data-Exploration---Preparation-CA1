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