## Continuing from Missingdata.R...
## Logistic Regression

setwd("C:/Users/holly/OneDrive/Documents/R/656")

## Loading required packages
require(dplyr)
require(readr)
require(caret)
require(pROC)
require(nnet)

## Using imp_data_tx from Missingdata.R file

## Exploratory Data Analysis
imp_data_tx %>%
  group_by(Stratification1) %>%
  summarise(n())

imp_data_tx %>%
  group_by(Stratification2) %>%
  summarise(n())

imp_data_tx %>%
  group_by(LocationDesc) %>%
  summarise(n())

imp_data_tx %>%
  filter(LocationDesc == "Anderson County")

## Data Splitting
## Using County as the stratifier
Y                 = imp_data_tx$Data_Value_Binned
set.seed(2)
trainingDataIndex = createDataPartition(imp_data_tx$LocationDesc, p = 0.5, list=F)
trainingData      = imp_data_tx[trainingDataIndex,]
testingData       = imp_data_tx[-trainingDataIndex,]

Xtrain            = select(trainingData, -Data_Value_Binned)
Xtest             = select(testingData, -Data_Value_Binned)
Ytrain            = factor(select(trainingData, Data_Value_Binned) %>% unlist(),
				labels = c('Very Low', 'Low', 'Moderate', 'High', 'Very High'))
Ytest             = factor(select(testingData, Data_Value_Binned) %>% unlist(),
				labels = c('Very Low', 'Low', 'Moderate', 'High', 'Very High'))


## Cleaning up memory
rm(trainingData)
rm(testingData)

## Want to be able to predict Heart Disease Mortality Rate based on
## gender, race/ethnicity, and geographic location
Xtrain            = select(Xtrain, Stratification1, Stratification2, LocationDesc) %>%
			  mutate_all(factor)
Xtest             = select(Xtest, Stratification1, Stratification2, LocationDesc) %>%
			  mutate_all(factor)

str(Xtrain)

sapply(Xtrain, levels)
sapply(Xtest, levels)

## Dummy variable coercion
dummyModel  = dummyVars(~ ., data = Xtrain, fullRank=T)

XtrainFull  = predict(dummyModel, Xtrain)
XtestFull   = predict(dummyModel, Xtest)

## Logistic Regression
YtrainRelevel = relevel(Ytrain, ref = 'Very High')
YtestRelevel  = relevel(Ytest, ref = 'Very High')

trControl = trainControl(method = 'none')
outLogistic = train(x = XtrainFull, y = YtrainRelevel, method = 'multinom', trControl = trControl)

