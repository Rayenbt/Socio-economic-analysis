library(tidyverse)
library(readxl)


data<- read_excel("WorldBank.xlsx")
head(data)


##### In the following lines of code we will explore the dataset, deal with missing values at first, especially the GDP related ones, 
##### and drop columns with high ratio of missing values ####

dim(data)
colnames(data)
summary(data)
### 12449 rows with 15 columns, few missing data across all the columns and rows, we need to inspect how is this distributed
glimpse(data)
### Inspecting the ratio of null values across all the columns to decide which rows to drop based on the needs and the null values
### Because we have 12k rows and we start from 1960 to 2018, it is normal that most of the missing data will be in the earlier years
na_per_year <- data %>%
  group_by(Year) %>%
  summarise_all(~mean(is.na(.))*100)
print(na_per_year, n=Inf, width=Inf)
### GDP is important for our analysis so we need to be careful, 15% missing in 1990 so we will start our analysis from 1990 to 2018 (23 in 1989)



#### Dropping some columns that are not useful for our analysis or Have high percentage of null values
data <- data %>%
  select(-contains("Electric power consumption (kWh per capita)"), ## 7241 null values out of 12449 rows
         -contains("Individuals using the Internet (% of population)") ## 7385 null values out of 12449 rows
         ) 
  
data <- data %>%
  filter(Year >= 1990)


na_data <- data %>%
  summarise_all(~mean(is.na(.))*100)

print(na_data,n=Inf,width = Inf)
dim(data)
### Now we have partially cleaned our data and we are left with 6119 rows and 13 columns


unique(data$`Country Name`) ## checking the number of unique countries

## countries that have no GDP, we counted the ratio of no gdp during the whole period, to eliminate countries where there are high absence of gdp
no_gdp_country <- data %>%
  group_by(`Country Name`) %>%
  summarise(
    total_years = n(),
    na_years = sum(is.na(`GDP (USD)`)),
    ratio_na = na_years  / total_years
  )
print(no_gdp_country, n= 211)

### our ratio will be 0.2, so we ended up with 183 countries
condition_ratio <- 0.2
filtered_countries <- no_gdp_country %>%
  filter (ratio_na <= condition_ratio)
  
print(filtered_countries, n = 183)

## we use the previous data frame and filter for countries that have a ratio lower than the 0.2 ratio
data <- data %>%
  filter(`Country Name` %in% filtered_countries$`Country Name`)

## We summarize again to deal 
no_gdp_country_2 <- data %>%
  group_by(`Country Name`) %>%
  summarise(
    total_years = n(),
    na_years = sum(is.na(`GDP (USD)`)),
    ratio_na = na_years  / total_years
  )
sum(no_gdp_country_2$na_years) 
## we are only left with 63 rows out of 5307 that don't have gdp