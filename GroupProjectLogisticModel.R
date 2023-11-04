## Continuing from Missingdata.R...
## Logistic Regression

## Loading required packages
require(dplyr)
require(readr)
require(caret)
require(pROC)

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
Y                 = imp_data_tx$Data_Value	## In example Y is qualitative
set.seed(2)
trainingDataIndex = createDataPartition(imp_data_tx$LocationDesc, p = 0.5, list=F)
trainingData      = imp_data_tx[trainingDataIndex,]
testingData       = imp_data_tx[-trainingDataIndex,]

Xtrain            = select(trainingData, -Data_Value)
Xtest             = select(testingData, -Data_Value)
Ytrain            = select(trainingData, Data_Value)
Ytrain            = as.numeric(Ytrain$Data_Value)
Ytest             = select(testingData, Data_Value)

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
## Left out releveling of Y done in class notes because our supervisor is not qualitative
trControl   = trainControl(method = 'none')
outLogistic = train(x = XtrainFull, y = Ytrain, method = 'glm',
			trControl = trControl)

YhatTestProb = predict(outLogistic, XtestFull, type = 'prob')

## Error in dimnames(out)[[2]] <- modelFit$obsLevels : 
## length of 'dimnames' [2] not equal to array extent


calibProbs = calibration(Ytest ~ YhatTestProb)
xyplot(calibProbs)