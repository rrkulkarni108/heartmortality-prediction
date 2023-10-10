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

