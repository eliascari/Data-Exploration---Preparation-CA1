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

#population from 2024
population_2024 <- population_clean %>%
  filter(
    Time == 2024,
    Variant == "Median",
    Sex == "Both sexes"
  )
#selecting the top 50 countries based on hightes to lowest population
#im doing this because the dataset is too large
top_50_locations <- population_2024 %>%
  filter(
    IndicatorName == "Total population by sex",
    Age == "Total"
  ) %>%
  arrange(desc(Value)) %>%
  slice_head(n = 50) %>%
  pull(Location)

length(top_50_locations)

#filtering and checking if the top 50 population was done correctly
population_top50 <- population_2024 %>%
  filter(Location %in% top_50_locations)

length(unique(population_top50$Location))

#fertility graph using the top 50 countries
fertility_top50 <- population_top50 %>%
  filter(
    IndicatorName == "Total fertility rate"
  )

ggplot(
  fertility_top50,
  aes(
    x = reorder(Location, Value),
    y = Value
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total Fertility Rate in the Top 50 Most Populous Locations (2024)",
    x = "Location",
    y = "Children per woman"
  ) +
  theme_minimal()


#life expectancy in the 50 countries with the most population
life_expectancy_top50 <- population_top50 %>%
  filter(IndicatorName == "Life expectancy at birth")

ggplot(
  life_expectancy_top50,
  aes(x = reorder(Location, Value), y = Value)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Life Expectancy in the Top 50 Most Populous Locations in 2024",
    x = "Location",
    y = "Life Expectancy (Years)"
  )

#infant mortality in the 50 countries with most population
infant_mortality_top50 <- population_top50 %>%
  filter(IndicatorName == "Infant mortality rate")

ggplot(
  infant_mortality_top50,
  aes(x = reorder(Location, Value), y = Value)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Infant Mortality in the Top 50 Most Populous Locations (2024)",
    x = "Location",
    y = "Infant Deaths per 1,000 Live Births"
  )

#comparison between fertility and life expectancy
fertility_life <- population_top50 %>%
  filter(IndicatorName %in% c(
    "Total fertility rate",
    "Life expectancy at birth"
  )) %>%
  select(Location, IndicatorName, Value) %>%
  pivot_wider(
    names_from = IndicatorName,
    values_from = Value
  ) %>%
  drop_na()

dim(fertility_life)

#this will check if there correlation between them
cor(
  fertility_life$`Total fertility rate`,
  fertility_life$`Life expectancy at birth`,
)

#scatter plot to check for the correlation
ggplot(
  fertility_life,
  aes(
    x = `Total fertility rate`,
    y = `Life expectancy at birth`
  )
) +
  geom_point() +
  labs(
    title = "Relationship Between Fertility and Life Expectancy",
    x = "Total Fertility Rate (Children per Woman)",
    y = "Life Expectancy (Years)"
  )


#highest fertility rate from the 5 highest countries
fertility_extremes <- fertility_life %>%
  arrange(desc(`Total fertility rate`))

head(fertility_extremes, 5)

#lowest fertility rate from the 5 lowest countries
tail(fertility_extremes, 5)

#highest infant mortality rate from the 5 highest countries
infant_extremes <- infant_mortality_top50 %>%
  arrange(desc(Value)) %>%
  select(Location, Value)

head(infant_extremes, 5)

#lowest infant mortality rate from the 5 lowest countries

tail(infant_extremes, 5)

#checking that each location has only 1 value for each indicator
pca_data <- population_top50 %>%
  filter(
    Age == "Total",
    IndicatorName %in% c(
      "Total fertility rate",
      "Life expectancy at birth",
      "Infant mortality rate (IMR)",
      "Under-five mortality rate (U5MR)",
      "Median age of population",
      "Crude birth rate",
      "Population density"
    )
  ) %>%
  select(Location, IndicatorName, Value)

#checking for duplicates in any location
pca_data %>%
  count(Location, IndicatorName) %>%
  filter(n > 1)

#making the data wide format for PCA
pca_wide <- pca_data %>%
  pivot_wider(
    names_from = IndicatorName,
    values_from = Value
  )

dim(pca_wide)

#standardisation so each variable it has approximately mean = 0, standard deviation = 1
pca_scaled <- scale(
  pca_wide %>%
    select(-Location)
)

#this will perform PCA using the standardised indicators
pca_result <- prcomp(
  pca_scaled,
  center = TRUE,
  scale. = TRUE
)

summary(pca_result)

