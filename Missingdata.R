#Continuing from Preprocessing.R....
##MICE Package

#Loading required package
require(mice)
require(VIM)

#Loading data
data <- read.csv("Heart_Disease_Mortality_Data_Among_US_Adults__35___by_State_Territory_and_County.csv", header= T)
data_dropped <- data[,-c(5, 6 , 7 , 10, 11 , 12, 17)]
data_tx <- data_dropped[data_dropped$LocationAbbr == "TX", ]

#Check which columns have missing values
colSums(is.na(data_tx))
#1726 missing values in Data_Value column

#percentage of missing data
perc <- function(x) {sum(is.na(x))/length(x)*100}
apply(data_tx, 2, perc)
#37.60349% of the values in Data_Value are missing

#Imputing
impute <- mice(data_tx, m=5, meth ="pmm",seed = 123)
summary(impute)

imp_data_tx <- complete(impute,1)

#Inspecting distribution of original data and imputed data
densityplot(impute)

#Creating bins 
bin_boundaries <- c(-Inf, 100, 200, 400, 800, Inf)
bin_labels <- c('Very low', 'Low', 'Moderate', 'High', 'Very high')
imp_data_tx$Data_Value_Binned <- cut(imp_data_tx$Data_Value, breaks = bin_boundaries, labels = bin_labels)

