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

## For the remaining null values, in order not to affect our analysis, we won't use mean/mode/median filling, instead we will use interpolation, further details will follow

## we started by filtering the countries with null GDP values
no_gdp_country_2 <- no_gdp_country_2 %>%
  filter(ratio_na>0)
no_gdp_country_2

## installed the necessary package
install.packages("zoo")
library(zoo)

## create a data frame test to handle filling missing values
colSums(is.na(data))
test <- data
test <- test %>%
  ## first we group the data with country names to keep high accuracy
  group_by(`Country Name`) %>%
  ## then we go through all the columns, we check first if it's all null values, if that's true we keep it as we will check it later
  ## if there is null values, we fill them through linear interpolation or extrapolation if the missing values are on the two sides of the data
  mutate(`GDP (USD)`=if (all(is.na(`GDP (USD)`))) `GDP (USD)` else na.approx(`GDP (USD)`,Year,rule=2)) %>% ## rule=2 because our values are on the extreme side: example: (Na,1,2,3,NA)
  mutate(`GDP per capita (USD)`=if (all(is.na(`GDP per capita (USD)`))) `GDP per capita (USD)` else na.approx(`GDP per capita (USD)`,Year,rule=2)) %>%
  mutate(`Death rate, crude (per 1,000 people)`=if (all(is.na(`Death rate, crude (per 1,000 people)`))) `Death rate, crude (per 1,000 people)` else na.approx(`Death rate, crude (per 1,000 people)`,Year,rule=2)) %>%
  mutate(`Infant mortality rate (per 1,000 live births)`=if (all(is.na(`Infant mortality rate (per 1,000 live births)`))) `Infant mortality rate (per 1,000 live births)` else na.approx(`Infant mortality rate (per 1,000 live births)`,Year,rule=2)) %>%
  mutate(`Life expectancy at birth (years)`=if (all(is.na(`Life expectancy at birth (years)`))) `Life expectancy at birth (years)` else na.approx(`Life expectancy at birth (years)`,Year,rule=2)) %>%
  mutate(`Unemployment (% of total labor force) (modeled ILO estimate)`=if (all(is.na(`Unemployment (% of total labor force) (modeled ILO estimate)`))) `Unemployment (% of total labor force) (modeled ILO estimate)` else na.approx(`Unemployment (% of total labor force) (modeled ILO estimate)`,Year,rule=2))%>%
  mutate(`Birth rate, crude (per 1,000 people)`=if (all(is.na(`Birth rate, crude (per 1,000 people)`))) `Birth rate, crude (per 1,000 people)` else na.approx(`Birth rate, crude (per 1,000 people)`,Year,rule=2)) %>% 
  mutate(`Population density (people per sq. km of land area)`=if (all(is.na(`Population density (people per sq. km of land area)`))) `Population density (people per sq. km of land area)` else na.approx(`Population density (people per sq. km of land area)`,Year,rule=2))


## once we finished, I created few dataframes that will check if the data still having null values, we take the relevant countries
## This should be relatively small countries so if they are removed our analysis wont' be too much affected

test_na_unemp <-test %>%
  filter(is.na(`Unemployment (% of total labor force) (modeled ILO estimate)`))%>%
  select(`Country Name`,Year,`Unemployment (% of total labor force) (modeled ILO estimate)`)
print(test_na_unemp)

test_na_inf <- test %>%
  filter(is.na(`Infant mortality rate (per 1,000 live births)`))%>%
  select(`Country Name`,Year,`Infant mortality rate (per 1,000 live births)`)
print(test_na_inf,n=203)

test_na_pop <- test %>%
  filter(is.na(`Population density (people per sq. km of land area)`))%>%
  select(`Country Name`,Year,`Population density (people per sq. km of land area)`)
print(test_na_pop,n=200)

## we add all the countries to one vector
countries_to_drop <-c(unique(test_na_unemp$`Country Name`) ,unique(test_na_inf$`Country Name`),unique(test_na_pop$`Country Name`))

## then we filter them out and end up with a clean df that has 4785 rows and 13 columns ( initial df had 12449 and 15 columns)
data_clean <- test %>%
  filter(!`Country Name` %in% countries_to_drop)

colSums(is.na(data_clean))
dim(data_clean)
