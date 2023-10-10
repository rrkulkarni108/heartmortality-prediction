#Loading required packages
require(dplyr)
require(ggplot2)

#Load in the dataset (csv. file added to repository already)
data <- read.csv("Heart_Disease_Mortality_Data_Among_US_Adults__35___by_State_Territory_and_County.csv", header= T)

#Looking at structure of the data
str(data)
dim(data)

#Observing first 5 rows
head(data, n = 5)

#Dropping nuisance variables
data_dropped <- data[,-c(5, 6 , 7 , 10, 11 , 12, 17)]
colnames(data_dropped) #Checking correct columns were dropped

#Filter for LocationAbbr = TX
data_tx <- data_dropped[data_dropped$LocationAbbr == "TX", ]
unique(data_tx$LocationAbbr) #Checking data is filtered for TX

#Checking structure of filtered data
str(data_tx)
dim(data_tx)

#Comparing change in dimensions
#Original data 59076 rows and 19 columns
#Filtered data 4590 rows and 13 columns 


#Exploratory Data Analysis (EDA)
summary(data_tx) #Summary statistics

#Observing counties
county_data <- data_tx %>%
  group_by(LocationDesc)%>%
  summarise(total_value = sum(Data_Value , na.rm= T))

#Arranging the counties based on total value (per 100,000 population)
arranged_county_data <- county_data %>%
  arrange(desc(total_value))

#Viewing the top 5 and bottom 5 counties in TX
head(arranged_county_data , n = 5)
tail(arranged_county_data , n = 5)

