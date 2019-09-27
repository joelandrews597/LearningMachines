reg=read.csv(file.choose())
cars = reg
str(cars)
summary(cars)

 
#Eliminate NEW_PRICE
cars$New_Price = NULL

cars$Name = NULL

#Replacing NA's with mean
replace_na = function(x){
  ifelse(is.na(x),mean(x,na.rm = T),x)
}
library(dplyr)
cars = cars %>% mutate_at(c("Mileage","Engine","Power","Seats"), replace_na)

cars$Transmission = ifelse(cars$Transmission=="Manual",1,0)

#Check skewness of the predictor variable
library(ggpubr)
ggdensity(cars$Price) #Right skewed
ggqqplot(cars$Price) #Heavy tailed
#Applying log10 transformation
cars$Price = log10(cars$Price)+1

#Checking correlation
num = sapply(cars,is.numeric)
num = cars[,num]
corrplot::corrplot(cor(num))


######Random Forest Model######
#Train/Test split
library(caTools)
set.seed(21)
splitIndex = sample.split(cars,SplitRatio = 0.7)
train = subset(cars,splitIndex==TRUE)
test = subset(cars,splitIndex==FALSE)

library(caret)
library(randomForest)
#Grid Search Algorithm
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(54)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(Price~., data=train, method="rf",tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
rf_gridsearch$finalModel
#The final value used for the model was mtry = 9.
#Rsquared - 0.9351 - 93.55%
#RMSE - 0.0963
#MAE - 0.0672


predrf_tr <- predict(rf_gridsearch, train) #Train Data
predrf_test <- predict(rf_gridsearch, test) #Test Data

#Calculate MSE
install.packages("Metrics")
library(Metrics)
mse(train$Price,predrf_tr)  #0.00206104      #(actual - predicted)^2
mse(test$Price,predrf_test) #0.01116391


#Load the actual test data
testcars = read.csv(file.choose())
testcars$Transmission=ifelse(testcars$Transmission=="Manual",1,0)
colSums(is.na(testcars))
testcars$New_Price = NULL

library(dplyr)
testcars = testcars %>% mutate_at(c("Engine","Power","Seats"), replace_na)

#Prediction for testcars
Price_test_cars = predict(rf_gridsearch,testcars)
PredictedValues = cbind(testcars,Price_test_cars)

library(ggpubr)
ggdensity(Price_test_cars)

write.xlsx(Price_test_cars, file = "C:\\Users\\user\\Desktop\\Business Analytics\\Linear Regression\\Participants_Data_Used_Cars\\SubmissionGS.xlsx", sheetName = "Predicted Prices", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
