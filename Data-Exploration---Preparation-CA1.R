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

#checking for the indicators names
sort(unique(population_clean$IndicatorName))

#observetions that every indicator has
population_clean %>%
  count(IndicatorName, sort = TRUE)

#checking whether records refer to diffent sexes, projection
#scenarios and estimation types
unique(population_clean$Sex)
unique(population_clean$Variant)
unique(population_clean$EstimateType)

#
class(population_clean$Value)

#checking for potential outliers using IQR method
outlier_summary <- population_clean %>%
  group_by(IndicatorName) %>%
  mutate(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR_Value = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR_Value,
    Upper_Bound = Q3 + 1.5 * IQR_Value
  ) %>%
  summarise(
    Observations = n(),
    Potential_Outliers = sum(
      Value < Lower_Bound | Value > Upper_Bound,
      na.rm = TRUE
    )
  )

outlier_summary




