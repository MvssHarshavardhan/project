setwd("F:\\data science\\data sets\\Project")
#install.packages("data.table")
library(data.table)

#loading data using fread
trainingdat <- fread("traindata.csv", stringsAsFactors = T)
testingdat <- fread("testdata.csv", stringsAsFactors = T)

#No.of rows and columns in trainset and testset
dim(trainingdat)

dim(testingdat)

# structure of the trainset
str(trainingdat)

#first prediction using mean
sub_mean<-data.frame(User_ID = testingdat$User_ID, Product_ID = testingdat$Product_ID, Purchase = mean(trainingdat$Purchase))
write.csv(sub_mean, file = "first_sub.csv", row.names = F)

#Summarize the datasets
summary(trainingdat)

summary(testingdat)

#combine data set
testingdat[,Purchase := mean(trainingdat$Purchase)]
c <- list(trainingdat, testingdat)
combinedata <- rbindlist(c)

#analyzing Age variable/ univariate analysis
combinedata[,prop.table(table(Age))]

#missing values
colSums(is.na(combinedata))

install.packages("ggplot2")
library(ggplot2)
ggplot(combinedata, aes(Age, fill = Gender))+ geom_bar()

library(dplyr)

#Data Manipulation using data.table, create a new variable for missing values
combinedata[,Product_Category_2_NA := ifelse(sapply(combinedata$Product_Category_2, is.na) ==    TRUE,1,0)]
combinedata[,Product_Category_3_NA := ifelse(sapply(combinedata$Product_Category_3, is.na) ==  TRUE,1,0)]

#impute missing values
combinedata[,Product_Category_2 := ifelse(is.na(Product_Category_2) == TRUE, "-999",  Product_Category_2)]
combinedata[,Product_Category_3 := ifelse(is.na(Product_Category_3) == TRUE, "-999",  Product_Category_3)]

#set column level
levels(combinedata$Stay_In_Current_City_Years)[levels(combinedata$Stay_In_Current_City_Years) ==  "4+"] <- "4"

#recoding age groups
levels(combinedata$Age)[levels(combinedata$Age) == "0-17"] <- 0
levels(combinedata$Age)[levels(combinedata$Age) == "18-25"] <- 1
levels(combinedata$Age)[levels(combinedata$Age) == "26-35"] <- 2
levels(combinedata$Age)[levels(combinedata$Age) == "36-45"] <- 3
levels(combinedata$Age)[levels(combinedata$Age) == "46-50"] <- 4
levels(combinedata$Age)[levels(combinedata$Age) == "51-55"] <- 5
levels(combinedata$Age)[levels(combinedata$Age) == "55+"] <- 6

#convert age to numeric
combinedata$Age <- as.numeric(combinedata$Age)

#convert Gender into numeric
combinedata[, Gender := as.numeric(as.factor(Gender)) - 1]

#User Count
combinedata[, User_Count := .N, by = User_ID]

#Product Count
combinedata[, Product_Count := .N, by = Product_ID]


#Mean Purchase of Product
combinedata[, Mean_Purchase_Product := mean(Purchase), by = Product_ID]

#Mean Purchase of User
combinedata[, Mean_Purchase_User := mean(Purchase), by = User_ID]


#hot encoding of City_Category variable
install.packages("dummies")
library(dummies)
combinedata <- dummy.data.frame(combinedata, names = c("City_Category"), sep = "_")

#check classes of all variables
sapply(combinedata, class)

#converting Product Category 2 & 3
combinedata$Product_Category_2 <- as.integer(combinedata$Product_Category_2)
combinedata$Product_Category_3 <- as.integer(combinedata$Product_Category_3)

#Model Building Begins

#Divide into train and test
c.train <- combinedata[1:nrow(trainingdat),]
c.test <- combinedata[-(1:nrow(trainingdat)),]

#Removing noise in Product_Category_1 variable
c.train <- c.train[c.train$Product_Category_1 <= 18,]

#Test with Multiple Regression model

# c.train <- c.train[c.train$Product_Category_1 <= 18,]
save.image("bala.rda")
load("bala.rda")
#write.csv(c.train,"C:\\Users\\Bal\\Desktop\\Assignments\\FirstAmericanPvtLtd\\train1.csv")
#write.csv(c.test,"C:\\Users\\Bal\\Desktop\\Assignments\\FirstAmericanPvtLtd\\test1.csv")

#random forest
library(randomForest)
rf <- randomForest(Purchase~.,data = c.train,ntree=100,ntry=3,importance = T)





