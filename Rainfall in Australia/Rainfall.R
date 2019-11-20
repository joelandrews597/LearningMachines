rain = read.csv(file.choose())

library(dplyr)
library(caret)
library(randomForest)
library(caTools)

str(rain)


####DATA CLEANING####

as.matrix(which(colSums(is.na(rain))>0)) 

#Removing cols Sunshine, Evaporation, Cloud9am, Cloud3pm
rain[,c(6,7,18,19)] = NULL

# We don't need the location column because 
# we are going to find if it will rain in Australia(not location specific)
# We are going to drop the date column too.
# We need to remove RISK_MM because we want to predict 'RainTomorrow' and RISK_MM can leak some info to our model
rain[,c("Date", "Location", "RISK_MM")] = NULL

#Replacing NA's with mean - numeric variables follow normal distribution
replace_na_mean = function(x){
  ifelse(is.na(x),mean(x,na.rm = TRUE),x)
}

rain = rain %>% mutate_at(c("MinTemp","MaxTemp","WindGustSpeed","WindSpeed9am","WindSpeed3pm",
                            "Humidity9am","Humidity3pm","Pressure9am","Pressure3pm",
                            "Temp9am","Temp3pm","Rainfall"),replace_na_mean)

#Replacing remaining NA's with mode
rain$WindGustDir[is.na(rain$WindGustDir)] = names(sort(-table(rain$WindGustDir)))[[1]]
rain$WindDir9am[is.na(rain$WindDir9am)] = names(sort(-table(rain$WindDir9am)))[[1]]
rain$WindDir3pm[is.na(rain$WindDir3pm)] = names(sort(-table(rain$WindDir3pm)))[[1]]
rain$RainToday[is.na(rain$RainToday)] = names(sort(-table(rain$RainToday)))[[1]]

as.matrix(which(colSums(is.na(rain))>0)) #No Missing Values


####DATA PREPROCESSING####

#Lets deal with the categorical cloumns now
# Changing yes/no to 1/0 for RainToday and RainTomorrow
rain$RainToday = ifelse(rain$RainToday == "Yes",1,0)
rain$RainTomorrow = ifelse(rain$RainTomorrow == "Yes",1,0)
rain$RainToday = as.factor(rain$RainToday)
rain$RainTomorrow = as.factor(rain$RainTomorrow)

#Splitting df into numeric and categorical
numeric_df = sapply(rain, is.numeric)
numeric_df = rain[,numeric_df]

categorical_df = sapply(rain,is.factor)
categorical_df = rain[,categorical_df] 

#Outlier detection and removal
#Using z-score standarization
numeric_df_scaled = scale(numeric_df)  
df_scaled = cbind(numeric_df_scaled,categorical_df)

df_scaled = as_tibble(df_scaled)
df_new = df_scaled %>% 
  filter_at(vars(c(1:12)),all_vars(.< 3))
dim(df_new)

#Min - Max Normalization
numeric_df_new = sapply(df_new, is.numeric)
numeric_df_new = df_new[,numeric_df_new]

NormNum = preProcess(numeric_df_new,method = "range")
DFNormNum = predict(NormNum, numeric_df_new)

#One hot encoding of categorical variables
categorical_df_new = sapply(df_new,is.factor)
categorical_df_new = df_new[,categorical_df_new] 

rainfeatures = categorical_df_new[,c(4,5)]  
categorical_df_new[,c(4,5)] = NULL

dummies = dummyVars(~., data = categorical_df_new)
dummies_categorical = predict(dummies,categorical_df_new)

#Combining DF
rain_combined = cbind(DFNormNum, dummies_categorical, rainfeatures)


#Splitting  into test/train
SplitIndex = sample.split(rain_combined, SplitRatio = 0.7)
X_train = subset(rain_combined, SplitIndex == "TRUE")
X_test = subset(rain_combined, SplitIndex == "FALSE")


####MODELLING####
#Random Forest 
modelrf = randomForest(RainTomorrow ~., data = X_train)
