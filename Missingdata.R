#Continuing from Preprocessing.R....
##MICE Package

#Loading required package
require(mice)
require(VIM)
require(ggmice)


#Loading data
#data <- read.csv("Heart_Disease_Mortality_Data_Among_US_Adults__35___by_State_Territory_and_County.csv", header= T)
gender_race_tx <- read.csv("gender_race_tx.csv", header= T)


#depreciated
#data_dropped <- data[,-c(5, 6 , 7 , 10, 11 , 12, 17)]
#data_tx <- data_dropped[data_dropped$LocationAbbr == "TX", ]

#Check which columns have missing values
colSums(is.na(gender_race_tx))
#1199 missing values in Data_Value column


#percentage of missing data
perc <- function(x) {sum(is.na(x))/length(x)*100}
apply(gender_race_tx, 2, perc)
#47.01961% of the values in DeathCount are missing

#checking which rows have NAs
gender_race_tx[rowSums(is.na(gender_race_tx))>0, ]

#checking how many NA DeathCount values in race/ethnicity American Indian and Alaskan Native 
AIAN = gender_race_tx[gender_race_tx$Race_Ethnicity == "American Indian and Alaskan Native",]
apply(AIAN, 2, perc)
#96.86275% of the values in DeathCount are missing
#we will drop these race/ethnicity American Indian and Alaskan Native from our dataset

#checking how many NA DeathCount values in race/ethnicity Asian and Pacific Islander 
AAPI = gender_race_tx[gender_race_tx$Race_Ethnicity == "Asian and Pacific Islander",]
apply(AAPI, 2, perc)
#80.78431% of the values in DeathCount are missing
#we will drop these race/ethnicity Asian and Pacific Islander from our dataset   


gender_race_tx <- gender_race_tx %>%  filter(Race_Ethnicity!='American Indian and Alaskan Native' & 
                             Race_Ethnicity!='Asian and Pacific Islander')

dim(gender_race_tx) #1530 rows,  6 cols

#checking how many NA DeathCount values in dataset with AAPI and AIAN removed 
apply(gender_race_tx, 2, perc)
#now only 19.15033% of the values in DeathCount are missing


#visualize missing values
md.pattern(gender_race_tx)


#We will now impute these missing DeathCount values using MICE

#Imputing
impute <- mice(gender_race_tx, m=5, meth ="pmm",seed = 123)
summary(impute)

imp_data_tx <- complete(impute,1)

#Inspecting distribution of original data and imputed data
densityplot(impute)


#some brief visualizations post-imputation

#visualize data after imputation by race/ethnicity
boxplot(as.numeric(imp_data_tx$DeathCount)~factor(imp_data_tx$Race_Ethnicity) )

#visualize data after imputation by gender
boxplot(as.numeric(imp_data_tx$DeathCount)~factor(imp_data_tx$Gender) )

#create numerical scores column of race/ethnicity for plotting
imp_data_tx$num_race_eth =  case_when(
  imp_data_tx$Race_Ethnicity == "White" ~ 1, 
  imp_data_tx$Race_Ethnicity == "Black" ~ 2,
  imp_data_tx$Race_Ethnicity == "Hispanic" ~ 3
)
#visualize data by 
plot(imp_data_tx$num_race_eth,as.numeric(imp_data_tx$DeathCount) )

#visualize the imputed values by race/ethnicity
ggmice(impute, aes(Race_Ethnicity,DeathCount ))+
  geom_point(alpha = 0.5)+labs(title = "Imputed Death Counts by Race/Ethnicity" )


#visualize the imputed values by gender
ggmice(impute, aes(Gender,DeathCount ))+
  geom_point(alpha = 0.5)+labs(title = "Imputed Death Counts by Gender" )


#########################################################################
#For multinomial logistic model:

#Creating bins 
bin_boundaries <- c(-Inf, 100, 200, 400, 800, Inf)
bin_labels <- c('Very low', 'Low', 'Moderate', 'High', 'Very high')
imp_data_tx$Data_Value_Binned <- cut(imp_data_tx$Data_Value, breaks = bin_boundaries, labels = bin_labels)




