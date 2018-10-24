#customer_behaviour

#importing data

library(data.table)
train <- read.csv("F:/data science/data sets/Project/traindata.csv",stringsAsFactors = T,na.strings = c(""," ",NA))
test <- read.csv("F:/data science/data sets/Project/testdata.csv",stringsAsFactors = T,na.strings = c(""," ",NA))

head(train)
sapply(train,class)

#taking 5% of sample in train dataset
#library(dplyr)
#train.sample <- sample_n(train,12000)
#sapply(train.sample,class)


#checking Missing values
sort(colSums(is.na(train)),decreasing = TRUE)

#Imputing Missing Values for training set
#install.packages("mice")
#library(mice)
#imputed_data <- mice(train.sample[,c(10,11)], m=2, maxit = 5, method = 'rf', seed = 500)
# max iterations :- maxit, and m = no.of times 


#train.sample[,c(10,11)] <- complete(imputed_data, 2)


#Replacing the missing values to not to loose the buying pattern
train$Product_Category_2 <- as.character(train$Product_Category_2)
train[which(is.na(train$Product_Category_2)==T),]$Product_Category_2 <- "Missing"
train$Product_Category_3 <- as.character(train$Product_Category_3)
train[which(is.na(train$Product_Category_3)==T),]$Product_Category_3 <- "Missing"
test$Product_Category_2 <- as.character(test$Product_Category_2)
test[which(is.na(test$Product_Category_2)==T),]$Product_Category_2 <- "Missing"
test$Product_Category_3 <- as.character(test$Product_Category_3)
test[which(is.na(test$Product_Category_3)==T),]$Product_Category_3 <- "Missing"

train$Marital_Status <- ifelse(train$Marital_Status==0,"NON Married","Married")
test$Marital_Status <- ifelse(test$Marital_Status==0,"NON Married","Married")

#convert variables as factors
train$Product_Category_1 <- as.factor(train$Product_Category_1)
test$Product_Category_1 <- as.factor(test$Product_Category_1)
train$Occupation <- as.factor(train$Occupation)
test$Occupation <- as.factor(test$Occupation)

#making classwise data sets
train <- data.frame(unclass(train))
train <- head(train,12000)
str(train)
test <- data.frame(unclass(test))
str(test)

#checking the missing values
sort(colSums(is.na(train)),decreasing = T)
sort(colSums(is.na(test)),decreasing = T)
unique(train$Marital_Status)
names(train)

summary(train)

#set column level
levels(train$Stay_In_Current_City_Years)[levels(train$Stay_In_Current_City_Years) ==  "4+"] <- "4"


#Multiple Regression model
var <- setdiff(names(train),c("Purchase","Product_ID","User_ID"))
Formula <- as.formula(paste('Purchase',paste(var,collapse = '+'),sep = '~'))

fit <- lm(Formula,data=train)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(fit)  # Plot the model information

summary(fit)
coef(summary(fit))
anova(fit)
plot(fit$residuals)

train1 <- train
train1$Marital_Status <- NULL


var <- setdiff(names(train1),c("Purchase","Product_ID","User_ID"))
Formula <- as.formula(paste('Purchase',paste(var,collapse = '+'),sep = '~'))

fit1 <- lm(Formula,data=train1)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(fit1)  # Plot the model information

summary(fit1)
coef(summary(fit1))
anova(fit1)
plot(fit1$residuals)

#prediction
#test$Purchase <- predict.lm(fit1,data=test)

#test$Purchase

#summary(test$Purchase)

#print(test$Purchase)



#Model validation
#rmse <- function(y, f) { sqrt(mean( (y-f)^2 )) }
#rmse(log(train1$Purchase,base=10),predict(fit1,newdata=train1))
#rmse(log(test$Purchase,base=10),predict(fit,newdata=test))
#coefficients(fit)


#random forest
library(randomForest)
rf <- randomForest(Formula,data = train1,ntree=100,ntry=3,importance = T)

#Predicting results
rf = predict(rf, newdata = train1)


#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=train1$Purchase, predicteds=rf)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy)

 

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds???actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  


#Calculate RMSE 
# summarize accuracy
rmse <- sqrt(mean((train1$Purchase - rf)^2))
print(rmse)



###################################################

#decision tree regression
# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
dt_regressor = rpart(formula = Purchase ~ Age+ Gender+Occupation+City_Category+Product_Category_1+Product_Category_2+Product_Category_3,
                     data = train1,
                     method = "anova",
                     control = rpart.control(minsplit = 3))

print(dt_regressor)
#Predicting results
dt_pred = predict(dt_regressor, newdata = train1)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=train1$Purchase, predicteds=dt_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) 

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds???actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  


#Calculate RMSE 
# summarize accuracy
rmse <- sqrt(mean((train1$Purchase - rf)^2))
print(rmse)
#2151
##################################################

#support vector regression
library(e1071)
svm_regressor = svm(formula = Purchase ~ Age+ Gender+Occupation+City_Category+Product_Category_1+Product_Category_2+Product_Category_3,data = train1)
                    
print(svm_regressor)
                    #data = train.sample,
                    #type = 'eps-regression',
                    #kernel = 'radial')

#Predicting the model
y_pred = predict(svm_regressor, newdata = train1)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=train1$Purchase, predicteds=y_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) 

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds???actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  


#Calculate RMSE 
# summarize accuracy
rmse <- sqrt(mean((train1$Purchase - dt_pred)^2))
print(rmse)



#################################### XGBOOST #################################################
library(xgboost)
#xg_classifier <- xgboost(data = as.matrix(trainm), label = trainm$Purchase, nrounds = 10)


install.packages("MatrixModels")
library(MatrixModels)
library(Matrix)
trainm <- sparse.model.matrix(Purchase ~ . -1,data=train1)
train_label <- train1[,"Purchase"]
train_matrix <- xgb.DMatrix(data= as.matrix(trainm),label = train_label)

library(xgboost)
xgbFit <- xgboost(data = as.matrix(trainm), label = train_label, nrounds = 10)

## Predictions
xgb_pred <- predict(xgbFit, newdata = as.matrix(trainm))

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=train_label, predicteds=xgb_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.8038814 highly positive correlation


#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds???actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
#0.772683
mape <- mean(abs((xgb_pred - actuals_preds$actuals))/actuals_preds$actuals) 
# 0.374517



#Calculate RMSE 
# summarize accuracy
rmse <- sqrt(mean((train_label - dt_pred)^2))
print(rmse)
# 3110.263


####################################################








