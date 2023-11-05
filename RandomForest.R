##Random Forest

library(randomForest)

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

rf_model <- randomForest(XtrainFull, Ytrain, ntree = 100, importance = TRUE)
predictions <- predict(rf_model, XtestFull)

##Visualizations
#Feature importance
varImpPlot(rf_model)

#Confusion matrix
conf_matrix <- confusionMatrix(predictions, Ytest)
print(conf_matrix)


