#reading the training data
data <- read.csv('train_u6lujuX.csv', header = TRUE)

# data cleanup and imputation on training data
data$Gender[data$Gender == ""] <- 'Male'
data$Loan_ID <- as.character(data$Loan_ID)
data$Married[data$Married == ""] <- 'No'
data$Dependents[data$Dependents == ""] <- '0'
data$Self_Employed[data$Self_Employed == ""] <- 'No'
data$Loan_Amount_Term[is.na(data$Loan_Amount_Term)] <- median(data$Loan_Amount_Term, na.rm = TRUE)
data$LoanAmount[is.na(data$LoanAmount)] <- median(data$LoanAmount, na.rm = TRUE)
data$Credit_History[is.na(data$Credit_History)] <- median(data$Credit_History, na.rm = TRUE)
data$Gender <- droplevels(data$Gender)
data$Married <- droplevels(data$Married)
data$Self_Employed <- droplevels(data$Self_Employed)
data$Dependents <- droplevels(data$Dependents)

#reading the test data 
test <- read.csv('test_Y3wMUE5.csv', header = TRUE)

# data cleanup and imputation on test data
test$Gender[test$Gender == ""] <- 'Male'
test$Loan_ID <- as.character(test$Loan_ID)
test$Married[test$Married == ""] <- 'No'
test$Dependents[test$Dependents == ""] <- '0'
test$Self_Employed[test$Self_Employed == ""] <- 'No'
test$Loan_Amount_Term[is.na(test$Loan_Amount_Term)] <- median(test$Loan_Amount_Term, na.rm = TRUE)
test$LoanAmount[is.na(test$LoanAmount)] <- median(test$LoanAmount, na.rm = TRUE)
test$Credit_History[is.na(test$Credit_History)] <- median(test$Credit_History, na.rm = TRUE)
test$Self_Employed <- test$Self_Employed[drop = TRUE]
test$Gender <- test$Gender[drop = TRUE]
test$Dependents <- test$Dependents[drop = TRUE]

#using mice package for data imputation (giving bad results)
#tempTestData <- mice(test,m=5,maxit=50,meth='pmm',seed=500)

library("party")
library(caret)
set.seed(333)
fit <- rpart(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area, data=data, method="class")
plot(fit)
text(fit)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

#controlling the splits
fit <- rpart(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area, data=data, method="class",control=rpart.control(minsplit=2, cp=0.01))
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
Prediction
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = Prediction)
write.csv(submit, file = "dtree.csv", row.names = FALSE)

# using random forest to improve the accuracy
library(randomForest)
fitRF <- randomForest(Loan_Status ~ Gender + Married + Dependents + Education
                      + Self_Employed + ApplicantIncome + CoapplicantIncome +
                        LoanAmount + Loan_Amount_Term + Credit_History + Property_Area,
                      data=data, importance=TRUE, ntree=2000)
varImpPlot(fitRF)
Prediction <- predict(fitRF, test)

#Implementing random forest and bagging ensemble algorithms
#utilizing conditional inference trees as base learners.
library(party)
set.seed(415)
fitCRF <- cforest(Loan_Status ~ Gender + Married + Dependents + Education
                  + Self_Employed + ApplicantIncome + CoapplicantIncome +
                    LoanAmount + Loan_Amount_Term + Credit_History + Property_Area
                  ,data = data, controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fitCRF, test, OOB=TRUE, type = "response")

submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = Prediction)
write.csv(submit, file = "FinalForest.csv", row.names = FALSE)

#implementing exteme gradient boosting(WIP)
#require(xgboost)
#bstSparse <- xgboost(data = data, label = data$Loan_Status , max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")
