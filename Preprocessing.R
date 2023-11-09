#Loading required packages
require(dplyr)
require(ggplot2)

#Load in the dataset (csv. file added to repository already)
data <- read.csv("Heart_Disease_Mortality_Data_Among_US_Adults__35___by_State_Territory_and_County.csv", header= T)

#Looking at structure of the data
str(data) #int, character and num columns
dim(data) #59076 obs. of  19 variables

#Observing first 5 rows
head(data, n = 5)

#Dropping nuisance variables
data_dropped <- data[,-c(5, 6 , 7 , 10, 11 , 12, 17)] 
colnames(data_dropped) #Checking correct columns were dropped

#Now we subset our data to be for the state of Texas
#Filter for LocationAbbr = TX
data_tx <- data_dropped[data_dropped$LocationAbbr == "TX", ]
unique(data_tx$LocationAbbr) #Checking data is filtered for TX

#Checking structure of filtered data
str(data_tx)
dim(data_tx) #4590 obs. of  12 variables

#Comparing change in dimensions
#Original data 59076 rows and 19 columns
#Filtered data 4590 rows and 12 columns 


#Exploratory Data Analysis (EDA)
summary(data_tx) #Summary statistics

View(data_tx)

#We can observe that in Data_value column there are 1726 NA's. 
#We will deal with them later.                       

##############################################################################
#We sum the death rate (Data_value) per 100,000 people so we have an aggregate
#for each county in Texas
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


#Group based on Gender
gender_data <- data_tx %>%
  group_by(Stratification1)%>%
  summarise(occurances = n(),
            total_value = sum(Data_Value , na.rm = T))

#Seems like there are an equal number of occurrences but a higher data value for Males compared to Women

subsetgenderrace <- data_tx %>%
  group_by(LocationDesc)%>%
  summarise(total_value = sum(Data_Value , na.rm= T))
############################################################################################################

#Check for missing data
sapply(data_tx, function(x) sum(is.na(x)))

#1726 missing values in Data_Value column
#Doesn't make sense to discard such a large number of samples
#Impute? We can use a prediction model to handle missing data

#visualize missing values
md.pattern(data_tx, rotate.names = TRUE)


#Since data for Texas is stratified using Gender and Race/Ethnicity, 
#and we have category of Overall which aggregates different combinations
#of the two strata, we will remove these rows which have Overall in either 
#stratus' column. 

gender_race_tx = data_tx[data_tx[, c(8)] != "Overall" & data_tx[, c(10)] != "Overall", ]
head(gender_race_tx)
dim(gender_race_tx)
#View(gender_race_tx)

#also will remove now the columns of Year, LocationAbbr, GeographicLevel, Data_Value_Unit, 
#StratificationCategory1, StratificationCategory2.
gender_race_tx <- gender_race_tx[,-c(1, 2 , 4 , 6, 7 , 9, 17)] 
colnames(gender_race_tx) #Checking correct columns were dropped

#rename columns to more descriptive names
new_colnames = c("County", "DeathCount", "Gender", "Race_Ethnicity", "LocationID", "LatLong" )
for (i in 1:length(colnames(gender_race_tx))){
  colnames(gender_race_tx)[i] = new_colnames[i]
}
colnames(gender_race_tx) #check colnames

View(gender_race_tx)
head(gender_race_tx)
dim(gender_race_tx)

#Comparing change in dimensions
#Original data 59076 rows and 19 columns
#Subsetted data 2550 rows and 6 columns
