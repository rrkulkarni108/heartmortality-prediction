


#load in raw original dataset
data <- read.csv("Heart_Disease_Mortality_Data_Among_US_Adults__35___by_State_Territory_and_County.csv", header= T)

data <- read.csv("C:/Users/kulra/Contacts/Desktop/heartmortality-prediction/Heart_Disease_Mortality_Data_Among_US_Adults__35___by_State_Territory_and_County.csv")

#Dropping nuisance variables
data_dropped <- data[,-c(5, 6 , 7 , 10, 11 , 12, 17)] 

#Now we subset our data to be for the state of Texas
#Filter for LocationAbbr = TX
data_tx <- data_dropped[data_dropped$LocationAbbr == "TX", ]

#Subset data for GLM fitting. 
#Goal: Poisson Rate GLM Regression and Multicategory Logit models used 
#to predict the mean deaths from Heart rate mortality across counties
#Since data for Texas is stratified using Gender and Race/Ethnicity, 
#and we have category of Overall which aggregates different combinations
#of the two strata, we will remove these rows which have Overall in either 
#stratus' column. 

gender_race_tx = data_tx[data_tx[, c(8)] != "Overall" & data_tx[, c(10)] != "Overall", ]
head(gender_race_tx)
dim(gender_race_tx)


#Fit GLM to raw data. Will update to conduct training model and testing after. 
population_unit = rep(100000, 2550)
out1 = glm(Data_Value~factor(Stratification1)+factor(Stratification2), offset = log(population_unit), family = poisson (link = "log"), 
           data = gender_race_tx)
summary(out1)