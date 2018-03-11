library(corrplot)
library(dplyr)
library(RColorBrewer)
library(rattle)
library(ROCR)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(rpart.plot)
library(rpart)
library(caTools)

# Importing the dataset
dataset = read.csv('hr_analytics.csv')
#sapply(dataset,class)

# Splitting the dataset into the Training set and Test set

set.seed(123)
split = sample.split(dataset$left, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# convert all feature to numeric
dataset$number_project=as.numeric(dataset$number_project)
dataset$average_montly_hours=as.numeric(dataset$average_montly_hours)
dataset$time_spend_company=as.numeric(dataset$time_spend_company)
dataset$Work_accident=as.numeric(dataset$Work_accident)
dataset$promotion_last_5years=as.numeric(dataset$promotion_last_5years)
dataset$sales=as.numeric(dataset$sales)
dataset$salary=as.numeric(dataset$salary)

#sapply(training_set,class)

classifier <- rpart(formula = left~.,
                       data = training_set,method = "class",
                    parms = list(split="gini"))

# Training on those columns with high co-relation

#classifier2 <- rpart(formula = left~ time_spend_company + satisfaction_level,
                     #data = training_set,method = "class")

# Decision Tree plot

fancyRpartPlot(classifier,cex=0.6)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-10],type="class")


# Confusion Matrix

#cm <- table(test_set$left,y_pred)
#print(cm)
confusionMatrix(test_set$left, y_pred)

# ROC Curve Before Pruning

par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred),as.numeric(test_set$left))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - Decision Tree")

printcp(classifier)
# Pruning the tree

dt1<-prune(classifier,cp=0.01)
rpart.plot(dt1,main = "Classification Tree for Predicting Employee attrition", cex=0.7)

asRules(dt1)

pred.p<- predict(dt1,newdata = test_set,type="class")
confusionMatrix(pred.p,test_set$left)

#Plotting RoC Curve after Pruning


pred1 <- prediction(as.numeric(pred.p),as.numeric(test_set$left))
perf<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(pred.p),as.numeric(test_set$left))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - Pruned Decision Tree")



