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

