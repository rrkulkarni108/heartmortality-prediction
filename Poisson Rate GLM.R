#Continuing from Missingdata.R....

#Loading required packages
require(dplyr)
require(ggplot2)
require(readr)
require(caret)
require(pROC)
require(lattice)
require(corrplot)
require(e1071)

#load in raw original dataset
imp_gender_race_tx <- read.csv("imp_gender_race_tx.csv", header= T)

set.seed(1)

#create ID variable
imp_gender_race_tx$id <- 1:nrow(imp_gender_race_tx)

#Use 70% of dataset as training set and remaining 30% as testing set 
train <- imp_gender_race_tx %>% dplyr::sample_frac(0.7)
test  <- dplyr::anti_join(imp_gender_race_tx, train, by = 'id')

# training set has 1785 rows, 7 columns (including id)
dim(train)

# testing set has 765 rows, 7 columns (including id)
dim(test)




#Subset data for GLM fitting. 
#Goal: Poisson Rate GLM Regression and Multicategory Logit models used 
#to predict the mean deaths from Heart rate mortality across counties

#Model the average heart rate deaths /100,000 people across Texas counties
#log(mu) = beta0 + beta1I(gender = Male) + beta2I(Race = Asian and Pacific Islander) +
#beta3*Race(Indicator = Black) + beta4*Race(Indicator = Hispanic) + beta5*Race(Indicator = Hispanic) + log(100000)
#Race = American Indian and Alaska Native as well as Gender = Female are in the intercept



#Fit GLM to raw data. Will update to conduct training model and testing after. 

#offset term
population_unit = rep(100000, nrow(imp_gender_race_tx))

#fit Poisson rate model
out1 = glm(DeathCount~factor(Gender)+factor(Race_Ethnicity), offset = log(population_unit), family = poisson (link = "log"), 
           data = imp_gender_race_tx)
summary(out1)
