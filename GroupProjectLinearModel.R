## Continuing from Missingdata.R...
## Linear Regression

setwd("C:/Users/holly/OneDrive/Documents/R/656")

## Loading required packages
require(dplyr)
require(ggplot2)
require(caret)
require(lattice)
require(corrplot)
require(e1071)

imp_gender_race_tx <- read.csv("imp_gender_race_tx.csv")

str(imp_gender_race_tx)

X = imp_gender_race_tx %>%
  select(Gender, Race_Ethnicity, County) %>%
  mutate_all(factor)

Y = log(imp_gender_race_tx$DeathCount)

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
