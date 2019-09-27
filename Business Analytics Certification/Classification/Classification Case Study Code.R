library(data.table)
library(reshape2)
library(randomForest)
library(party)      # FOr decision tree 
library(rpart)      # for Rpart
library(rpart.plot) #for Rpart plot
library(lattice)    # Used for Data Visualization
require(caret)      # for data pre-processing
library(pROC) 
library(corrplot)   # for correlation plot
library(e1071)      # for ROC curve
library(RColorBrewer)

#Reading Data
hr_data = read.csv(file.choose())
#hr_data = choose.files()
View(hr_data)

names(hr_data)

#Removing role & salary; using role_code & salary_code
hr_data1 = hr_data[,-c(9,10)]

hr_data1$role_code = as.factor(hr_data1$role_code)
hr_data1$salary.code = as.factor(hr_data1$salary.code)

View(hr_data1)

summary(hr_data1)

boxplot(hr_data1$satisfaction_level ~ hr_data1$left, main = "Boxplot for Satisfaction_level")
#Obvious that people who left company have low satisfactions levels

boxplot(hr_data1$average_montly_hours ~ hr_data1$left, main = "Boxplot for Avg Monthly Hours")
#Looks like people who left had been putting more hours at work

boxplot(hr_data1$time_spend_company ~ hr_data1$left, main = "Boxplot for TIme Spent with Company")
#Something interesting that people wih longer term seem to be leaving the company. Either they could have reached a montonous work which could no longer be challenging for them so they leave.

#EDA
mosaicplot(hr_data1$left~hr_data1$salary.code, color='skyblue')
# From the plot it can be seen majority of the people leaving are with salary code=1

mosaicplot(hr_data1$left~hr_data1$promotion_last_5years, color='skyblue')
# Majority of them who left did not get any promotion in the last 5 years

mosaicplot(hr_data1$left~hr_data1$role_code, color='skyblue')

# Checking correlation
cor(hr_data1[,1:8])
library(corrplot)
corrplot(cor(hr_data1[,1:8]), method="circle")

# Creating train and test samples
set.seed(1234) #To ensure the repeatability of results
install.packages("createDataPartition")
splitIndex <- createDataPartition(hr_data1$left, p = .70,list = FALSE, times = 1)
trainSplit <- hr_data1[ splitIndex,]
testSplit <- hr_data1[-splitIndex,]
print(table(trainSplit$left))
print(table(testSplit$left))

#Check for the event rate
prop.table(table(trainSplit$left))
prop.table(table(testSplit$left))

# DT using rpart algorithm
?rpart()
fit = rpart(left ~ ., data = trainSplit,method = "class", control = rpart.control(minsplit = 30,cp = 0.01))
rpart.plot(fit)

print(fit)
summary(fit)

prp(fit)
plotcp(fit)
printcp(fit)


#Checking Confusion matrix on Train data
predtr <- predict(fit,trainSplit,type = "class")
library(caret)
library(InformationValue)
confusionMatrix(predtr,trainSplit$left)

#Checking COnfusion matrix on Test data
predtest <- predict(fit,testSplit, type = "class" )
confusionMatrix(predtest,testSplit$left)

# roc(testdata, prediction)
auctrain <- roc(as.numeric(trainSplit$left), as.numeric(predtr))
auctest <- roc(as.numeric(testSplit$left), as.numeric(predtest))
print(auctrain)
print(auctest)

?plot
plot(auctrain, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auctrain$auc[[1]],3)),col = 'blue')
# plot(auctrain,col = 'blue',main=paste('AUC:',round(auctrain$auc[[1]],3)))

###########Worst case AUC = 0.5#############

plot(auctest, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auctest$auc[[1]],3)),col = 'blue')
# plot(auctest,col = 'blue',main=paste('AUC:',round(auctest$auc[[1]],3)))




