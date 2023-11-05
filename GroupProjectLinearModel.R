## Continuing from Missingdata.R...
## Linear Regression

## Loading required packages
require(dplyr)
require(ggplot2)
require(caret)
require(lattice)
require(corrplot)
require(e1071)

str(imp_data_tx)

X = imp_data_tx %>%
  select(Stratification1, Stratification2, LocationDesc) %>%
  mutate_all(factor)

Y = imp_data_tx$Data_Value

dummyModel = dummyVars(~ ., data = X, fullRank=T)
Xdummy     = predict(dummyModel, X)

trControl = trainControl(method = 'cv', number = 10)
lmOut     = train(x = Xdummy, y = Y, method = 'lm', trControl = trControl)

Yhat = predict(lmOut, Xdummy)

residuals = Y - Yhat
residualPlotData = data.frame(residuals, Yhat)
ggplot(data = residualPlotData) +
	geom_point(aes(x = Yhat, y = residuals)) +
	geom_hline(yintercept = 0, color = 'red')
