train = read.csv(file.choose())
test = read.csv(file.choose())

#Libraries
library(plyr) #revalue()
library(knitr)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)

test_labels = test$Id
test$Id = NULL
train$Id = NULL

#Combining the two df.s for pre processing
test$SalePrice = NA
house = rbind(train,test)

#Checking for numeric variables
Num = sapply(house,is.numeric)
Num = house[,Num]
dim(Num)
Numvar_names = names(Num) #37 Numeric variables

#Correlation with SalePrice (>0.5)
Corr = cor(Num, use="pairwise.complete.obs")
Corr_sorted = as.matrix(sort(Corr[,'SalePrice'], decreasing = TRUE))
High_Corr = names(which(apply(Corr_sorted, 1, function(x) abs(x)>0.5)))
Corr_new = Corr[High_Corr,High_Corr]
Corr_newPlot = corrplot.mixed(Corr_new, tl.pos = "lt")
# 7 variables with corr > 0.5

#Check NA's
as.matrix(which(colSums(is.na(house))>0))

######### MISSING VALUE IMPUTATION #########
#Delete cols having large no.of missing values
house$Alley = NULL 
house$PoolQC = NULL
house$Fence = NULL
house$MiscFeature = NULL


#Impute NA's with 0
replace_na_0 = function (x){
  ifelse(is.na(x),0, x)
}

house = house %>% mutate_at(c("MasVnrArea","GarageCars","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF",
                              "BsmtFullBath","BsmtHalfBath"),replace_na_0)

#GarageYrBlt is the same as YearBuilt so, replacing NA's with YearBuilt
house$GarageYrBlt[is.na(house$GarageYrBlt)] = house$YearBuilt[is.na(house$GarageYrBlt)]


#Impute NA's with Mode
house$MSZoning[is.na(house$MSZoning)] = names(sort(-table(house$MSZoning)))[1]
house$MSZoning = as.factor(house$MSZoning)
house$Electrical[is.na(house$Electrical)] = names(sort(-table(house$Electrical)))[1]
house$Electrical = as.factor(house$Electrical)
house$Exterior1st[is.na(house$Exterior1st)] = names(sort(-table(house$Exterior1st)))[1]
house$Exterior1st = as.factor(house$Exterior1st)
house$Exterior2nd[is.na(house$Exterior2nd)] = names(sort(-table(house$Exterior2nd)))[1]
house$Exterior2nd = as.factor(house$Exterior2nd)
house$KitchenQual[is.na(house$KitchenQual)] = names(sort(-table(house$KitchenQual)))[1]
house$KitchenQual = as.factor(house$KitchenQual)
house$Functional[is.na(house$Functional)] = names(sort(-table(house$Functional)))[1]
house$Functional = as.factor(house$Functional)
house$SaleType[is.na(house$SaleType)] = names(sort(-table(house$SaleType)))[1]
house$SaleType = as.factor(house$SaleType)
house$MasVnrType[is.na(house$MasVnrType)] = names(sort(-table(house$MasVnrType)))[2]
house$MasVnrType = as.factor(house$MasVnrType)

table(house$Utilities, useNA = "ifany") #Only 2 NA's - AllPubs a majority class
#Therefore remove the column
house$Utilities = NULL

as.matrix(which(colSums(is.na(house))>0))

#Impute NA's with mean
house$LotFrontage[is.na(house$LotFrontage)] = 70.5
house$GarageArea[is.na(house$GarageArea)] = 472.9

table(house$FireplaceQu, useNA = "ifany") #47.26027% of data missing - but an important variable

#Impute NA's with "None"
replace_na = function (x){
  ifelse(is.na(x), "None", x)
}
#Converting to character
house = house %>% mutate_at(c("BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu",
                              "GarageType","GarageFinish","GarageQual","GarageCond"), as.character)
#Imputuing NA with "None"
house = house %>% mutate_at(c("BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu",
                                    "GarageType","GarageFinish","GarageQual","GarageCond"), replace_na)
#Converting back to factor
house = house %>% mutate_at(c("BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu",
                              "GarageType","GarageFinish","GarageQual","GarageCond"), as.factor)



######### ORDINALITY CHECK #########

Masonry = c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
house$MasVnrType=as.integer(revalue(house$MasVnrType, Masonry))

Qualities = c('None' = 0, 'Po' = 1,  'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
house$FireplaceQu = as.integer(revalue(house$FireplaceQu, Qualities))

Finish = c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
house$GarageFinish=as.integer(revalue(house$GarageFinish, Finish))

house$GarageQual=as.integer(revalue(house$GarageQual, Qualities))
house$GarageCond=as.integer(revalue(house$GarageCond, Qualities))

house$BsmtQual=as.integer(revalue(house$BsmtQual, Qualities))
house$BsmtCond=as.integer(revalue(house$BsmtCond, Qualities))

Exposure = c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
house$BsmtExposure=as.integer(revalue(house$BsmtExposure, Exposure))

FinType = c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
house$BsmtFinType1=as.integer(revalue(house$BsmtFinType1, FinType))
house$BsmtFinType2=as.integer(revalue(house$BsmtFinType2, FinType))

house$ExterQual=as.integer(revalue(house$ExterQual, Qualities))
house$ExterCond=as.integer(revalue(house$ExterCond, Qualities))

Shape = c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)
house$LotShape=as.integer(revalue(house$LotShape,Shape))

house$KitchenQual=as.integer(revalue(house$KitchenQual, Qualities))

Func = c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)
house$Functional = as.integer(revalue(house$Functional, Func))

house$HeatingQC = as.integer(revalue(house$HeatingQC, Qualities))
house$CentralAir = as.integer(revalue(house$CentralAir, c('N'=0, 'Y'=1)))

Slope =  c('Sev'=0, 'Mod'=1, 'Gtl'=2)
house$LandSlope = as.integer(revalue(house$LandSlope,Slope))

house$Street=as.integer(revalue(house$Street, c('Grvl'=0, 'Pave'=1)))

house$PavedDrive=as.integer(revalue(house$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))

house$MoSold = as.factor(house$MoSold)

house$MSSubClass = as.factor(house$MSSubClass)
MS = c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', 
            '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', '85'='split foyer', 
              '90'='duplex all style/age', '120'='1 story PUD 1946+', '150'='1,5 story PUD all', 
                '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion')
house$MSSubClass=revalue(house$MSSubClass, MS)


which(colSums(is.na(house))>0) #Just SalePrice


######### FEATURE ENGINEERING #########

house$Total_Bath = house$FullBath + (house$HalfBath*0.5) + house$BsmtFullBath + (house$BsmtHalfBath*0.5)

house$TotalSqFeet = house$GrLivArea + house$TotalBsmtSF

house$TotalPorchSF = house$OpenPorchSF + house$EnclosedPorch + house$X3SsnPorch + house$ScreenPorch

house$Remod = ifelse(house$YearBuilt==house$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
house$Age = as.numeric(house$YrSold)-house$YearRemodAdd
house$IsNew = ifelse(house$YrSold==house$YearBuilt, 1, 0) 
house$YrSold = as.factor(house$YrSold)


######### PRE-PROCESSING #########
#Dropping highly correlated variables - check correlation
drop = c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')
house = house[,!(names(house) %in% drop)]

#Seperating numeric variables
Numvar_names = Numvar_names[!(Numvar_names %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))]
Numvar_names = append(Numvar_names, c('TotalPorchSF', 'Total_Bath', 'TotalSqFeet','Age'))
Numeric_DF = house[,names(house) %in% Numvar_names]
Factor_DF = house[,!(names(house) %in% Numvar_names)]
Factor_DF = Factor_DF[, names(Factor_DF) != 'SalePrice']

length(Numeric_DF) #30 variables
length(Factor_DF) #44 variables

str(Numeric_DF) 
str(Factor_DF)

#Check skewness of the predictor variable 
library(ggpubr)
ggqqplot(house$SalePrice) #heavily skewed
ggdensity(house$SalePrice) #Right tailed - log transformation

#Log Tranformation to convert SalePrice to normal dist.
boxplot(house$SalePrice) #contains outliers - log transformation
house$SalePrice = log(house$SalePrice)
ggqqplot(house$SalePrice)
ggdensity(house$SalePrice)#normal distribution


#Skewness of numeric variables
# determine skew for each numeric feature
library(e1071)
for(i in 1:ncol(Numeric_DF)){
  if (abs(skew(Numeric_DF[,i]))>0.75){
    Numeric_DF[,i] <- log(Numeric_DF[,i] +1)
  }
}

#Normalization of data
NormNum = preProcess(Numeric_DF, method=c("center", "scale"))
print(NormNum)

DFnorm = predict(NormNum, Numeric_DF)
dim(DFnorm)

#Creating dummies for categorical variables
dummies = dummyVars(~.,Factor_DF)
categorical_1_hot = predict(dummies,Factor_DF)
dim(categorical_1_hot)


#Reconstruct new DF with pre-processed data
House_Clean = cbind(DFnorm,categorical_1_hot)
dim(House_Clean)

#Create Train/Test split
train_new = House_Clean[!is.na(house$SalePrice),]
test_new = House_Clean[is.na(house$SalePrice),]


######### MODELLING #########
### LASSO L1 REGULARIZATION
ctrl = trainControl(method = "repeatedcv", number = 5, repeats = 5, verboseIter=FALSE)
lambdas = seq(0.001,0.1,by = 0.0005)
set.seed(1544)
lasso_model = train(train_new, y = house$SalePrice[!is.na(house$SalePrice)], method = "glmnet", 
                    metric = "RMSE", maximize= F, trControl=ctrl,
                      tuneGrid = expand.grid(alpha=1, lambda = lambdas))
lasso_model
lasso_model$bestTune
min(lasso_model$results$RMSE)


mean(lasso_model$resample$RMSE)

LassoPred = predict(lasso_model, test_new)
predictions_lasso = exp(LassoPred) #need to reverse the log to the real values
head(predictions_lasso)

lassoVarImp = varImp(lasso_model,scale=F)
lassoImportance = lassoVarImp$importance

varsSelected = length(which(lassoImportance$Overall!=0))
varsNotSelected = length(which(lassoImportance$Overall==0))

cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')


solution = data.frame(Id=as.integer(rownames(test_new)),SalePrice=predictions_lasso)
write.csv(solution, file = "C:\\Users\\hp\\Desktop\\Business Analytics\\house-prices-advanced-regression-techniques\\Submissions1.csv", row.names = FALSE)

