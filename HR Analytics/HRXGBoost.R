# Importing the dataset
dataset = read.csv('hr_analytics.csv')


# Encoding the categorical variables as factors
dataset$sales = as.numeric(factor(dataset$sales,
                                  levels = c('accounting', 'hr', 'it','management','marketing','product_mng','RanD','sales','support','technical'),
                                  labels = c(1, 2, 3,4,5,6,7,8,9,10)))
dataset$salary = as.numeric(factor(dataset$salary,
                                   levels = c('low', 'medium','high'),
                                   labels = c(1, 2,3)))

# Encoding the target feature as factor
#dataset$left=as.numeric(dataset$left)
sapply(dataset,class)
dataset$number_project=as.numeric(dataset$number_project)
dataset$average_montly_hours=as.numeric(dataset$average_montly_hours)
dataset$time_spend_company=as.numeric(dataset$time_spend_company)
dataset$Work_accident=as.numeric(dataset$Work_accident)
dataset$promotion_last_5years=as.numeric(dataset$promotion_last_5years)
dataset$sales=as.numeric(dataset$sales)
dataset$salary=as.numeric(dataset$salary)



# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$left, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Fitting XGBoost to the Training set
install.packages('xgboost')
library(xgboost)
classifier = xgboost(data = as.matrix(training_set[-10]), label = training_set$left, nrounds = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(test_set[-10]))
y_pred = (y_pred >= 0.5)
y_pred=as.matrix(y_pred)
y_pred =y_pred +0

# Making the Confusion Matrix
library(caret)
cm = confusionMatrix(test_set[,10], y_pred)
cm

#ROC before pruning
library(ROCR)
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred),as.numeric(test_set$left))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - XgBoost")



#pruning the xgboost model:
# Fitting XGBoost to the Training set
install.packages('xgboost')
library(xgboost)
classifier2x = xgboost(data = as.matrix(training_set[-10]), label = training_set$left,params = list(gamma=1),nrounds = 10)

# Predicting the Test set results
y_pred2x = predict(classifier2x, newdata = as.matrix(test_set[-10]))
y_pred2x = (y_pred2x >= 0.5)
y_pred2x=as.matrix(y_pred2x)
y_pred2x =y_pred2x +0

# Making the Confusion Matrix
library(caret)
cm = confusionMatrix(test_set[,10], y_pred2x)
cm

#ROC after pruning
library(ROCR)
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred2x),as.numeric(test_set$left))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - XgBoost after pruning")


