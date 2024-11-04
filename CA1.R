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


#### After considering my data, I decide to broaden my dataset and merge it with another data for my analysis
data_2 <- read_csv("Countries.csv")
countries <- unique(data_clean$`Country Code`)

data_2_filtered <- data_2 %>%
  filter(`Country Code` %in% countries)

dim(data_2_filtered)  
head(data_2_filtered) 
str(data_2_filtered)

selected_cols <- c("Country Name","Country Code","Year","Inflation Rate","Agriculture (% GDP)","Industry (% GDP)","Service (% GDP)","Health Expenditure (% GDP)","Education Expenditure (% GDP)","Population"
)

data_2_filtered <- data_2_filtered %>%
  filter(Year <= 2018) %>%
  select(all_of(selected_cols))

colSums(is.na(data_2_filtered))

## same approach, I have taken just few columns of this data and made sure it's within the max years range (2018)
## Now I will clean some missing values, I only have three columns with missing values so I will discuss what I'm going to do for each one

inf_emp <- data_2_filtered %>%
  filter(is.na(`Inflation Rate`))
print(inf_emp,n=60)  

health_emp <- data_2_filtered %>%
  filter(is.na(`Health Expenditure (% GDP)`))
print(health_emp,n=60)


education_emp <- data_2_filtered %>%
  filter(is.na(`Education Expenditure (% GDP)`))
print(education_emp,n=60)

### Searched the internet for the values and I got them from this website: https://www.imf.org/external/datamapper/PCPIPCH@WEO/OEMDC/ADVEC/WEOWORLD/WSM/ARG/CUB/BIH
argentina_inf <- c(-0.9,-1.1,25.9,13.4,4.4,9.6,10.9,8.8,8.6,6.3,10.5,9.8,10,10.6,NA,NA,NA,25.7,34.3)
turkmenistan_inf <- c(8,11.6,8.8,5.6,5.9,10.7,8.2,6.3,14.5,-2.7,4.4,5.3,5.3,6.8,6,7.4,3.6,8,13.3)
cuba_inf <- c(NA,NA,NA,NA,NA,3.7,5.7,2.8,0.8,3.1,2.1,4.8,5.5,6,5.3,4.2,4.5,5.5,6.9)
### Values extracted from Palestinian central bureau of statistics: https://www.pcbs.gov.ps/statisticsIndicatorsTables.aspx?lang=en&table_id=3453
palestine_health <- c(9.7,10.5,11.4,9.9,11.7,11.3,10.8,10,10.8,11,9.3,11.7,10.5,10.6,10,9.5,9.1,9.3,9.7)

##Just filled the missing values with the previous vectors 
data_2_filtered <- data_2_filtered %>%
  mutate(`Inflation Rate` = ifelse(`Country Name` == "Argentina", argentina_inf, `Inflation Rate`)) %>%
  mutate(`Inflation Rate` = ifelse(`Country Name` == "Turkmenistan", turkmenistan_inf, `Inflation Rate`)) %>%
  mutate(`Inflation Rate` = ifelse(`Country Name` == "Cuba", cuba_inf, `Inflation Rate`)) %>%
  mutate(`Health Expenditure (% GDP)`= ifelse(`Country Code`=="PSE",palestine_health,`Health Expenditure (% GDP)`))

colSums(is.na(data_2_filtered))

### Cuba was missing some values for the first few years so I used interpolation to fill it, 
### so the NA'S will be replaced with the first value which is 3.7
data_2_filtered <- data_2_filtered %>%
  group_by(`Country Name`) %>%
  mutate(`Inflation Rate` = na.approx(`Inflation Rate`, na.rm = FALSE,rule=2)) %>%
  ungroup()

colSums(is.na(data_2_filtered))

### For the education expenditure, no results on the internet so we will fill missing values with mean for the 3 countries
data_2_filtered <- data_2_filtered %>%
  group_by(Year) %>%
  mutate(`Education Expenditure (% GDP)` = ifelse(is.na(`Education Expenditure (% GDP)`) & 
                                                    `Country Name` %in% c("Libya", "Bosnia and Herzegovina", "Equatorial Guinea"),
                                                  mean(`Education Expenditure (% GDP)`, na.rm = TRUE),
                                                  `Education Expenditure (% GDP)`)) %>%
  ungroup()

colSums(is.na(data_2_filtered))


### Now our second data set is clean and we are ready to merge
### Note: because the old data had time range from 1990 to 2018 and the new one starts from 2000,
### We will make our analysis from 2000 to 2018

final_data <- merge(data_clean,data_2_filtered,by=c("Country Name","Country Code","Year"))
dim(final_data)
colSums(is.na(final_data))
str(final_data)
summary(final_data)


### First, we take only numeric columns from our data frame
long_final_data <- final_data %>%
  select(where(is.numeric)) %>%
### Then we change the values to long format because according to the research I have done when I was looking to plot all
### columns, it's a good practice to follow long format especially because ggplot works better with long data
### The code was inspired by this subreddit: https://www.reddit.com/r/rstats/comments/l2moot/how_do_i_create_a_histogram_with_two_columns_of_a/
### and this reference on stack overflow: https://stackoverflow.com/questions/59778337/how-to-pivot-longer-a-set-of-multiple-columns-and-how-to-go-back-from-that-long 
  pivot_longer(cols=everything(),names_to = "Column", values_to = "Value")

ggplot(long_final_data,aes(x=Value))+
  geom_histogram(bins=30, fill="skyblue",color="black")+
  facet_wrap(~ Column, scales="free")+
  labs(title="Histogram of all numeric columns in the data",
       x= "Value",
       y="Frequency")+
  theme_light()


####################################
## CORRELATION MATRIX AND HEATMAP ##
####################################

## Just followed what we discoverd in week 6 tutorial to make a correlation
## matrix and then display it in a heatmap
library(caret)
library(ggplot2)
library(FSelector)

numeric_final_data <- final_data[sapply(final_data,is.numeric)]
str(numeric_final_data)
correlationMatrix <- cor(numeric_final_data)
correlationMatrix

highlyCorrelated <- findCorrelation(correlationMatrix,cutoff = 0.5)
highlyCorrelated

subset <- numeric_final_data[,c(highlyCorrelated)]
head(subset)

redundant_features <-names(subset)
redundant_features

correlationMatrix <- cor(subset)
correlationMatrix

round2 <- function(x){
  sprintf("%.2f",x)
}
library(gplots)

dev.new()
heatmap.2(
  correlationMatrix,
  symm = TRUE,
  trace = "none",
  col = colorRampPalette(c("blue", "white", "red"))(50),
  ### the number 50 specifies the number of color shades that should be generated by the color palette
  main = "Correlation Heatmap",
  xlab = "Features",
  ylab = "Features",
  scale = "none",
  margins = c(15, 15),
  cellnote = matrix(round2(correlationMatrix), ncol = ncol(correlationMatrix)),
  notecol = "black",
  notecex = 1.2,
  symbreaks = FALSE,
  labRow = redundant_features,
  labCol = redundant_features,
  density.info = "none",
  key = TRUE,
  cexCol = 0.9,
  ## Adjust font size for column names
  cexRow = 0.9
) ## Adjust font size for row names


