#setwd("C:/AML - BUAN 6341'")
setwd('/Users/vishnupriyakodavatiganti/Desktop/P3/R project files/hr_analytics')


install.packages('caTools')
install.packages('caret')
install.packages('lattice')
install.packages('ggplot2')
install.packages('e1071')
install.packages('corrplot')
install.packages('dplyr')
install.packages('RColorBrewer')
install.packages('rattle')
install.packages('ROCR')
install.packages('h2o')

library(corrplot)
library(dplyr)
library(RColorBrewer)
library(rattle)
library(ROCR)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(caTools)
# Importing the dataset
dataset = read.csv('hr_analytics.csv')

# Encoding the categorical variables as factors
dataset$sales = as.numeric(factor(dataset$sales,
                                  levels = c('accounting', 'hr', 'it','management','marketing','product_mng','RanD','sales','support','technical'),
                                  labels = c(1, 2, 3,4,5,6,7,8,9,10)))
dataset$salary = as.numeric(factor(dataset$salary,
                                   levels = c('low', 'medium','high'),
                                   labels = c(1, 2,3)))

# Set the target variable as a factor
dataset$left <- as.factor(dataset$left)
dataset=na.omit(dataset)
#splitting the data
set.seed(123)
split = sample.split(dataset$left, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-10] = scale(training_set[-10])
test_set[-10] = scale(test_set[-10])

#install.packages('class')
library(class)
train=training_set[,-10]
test=test_set[,-10]
cl=training_set[,10]
#cross validation
y_pred_cv=knn.cv(train,cl,k=3,prob =FALSE)
cm = confusionMatrix(cl, y_pred_cv)
cm


#prediction on random model k=3
y_pred_r=knn(train,test,cl,k=3,prob=TRUE)
cm12 = confusionMatrix(test_set[,10], y_pred_r)
cm12

# ROC Curve KNN
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred_r),as.numeric(test_set$left))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - 3Knn")

#training loop cv
accuracy=rep(0,10)
for(i in 1:10)
{
y_pred_cv=knn.cv(train,cl,k=i,prob=FALSE)
t1 = table(cl, y_pred_cv)
accuracy[i]=(t1[1]+t1[4])/(t1[1]+t1[2]+t1[3]+t1[4])*100
}
accuracy


#plot accuracy vs neigh
plot(accuracy,type="o",xlab="Number of neighbours",ylab="Accuracy")

#prediction on best model k=1
y_pred=knn(train,test,cl,k=1,prob=TRUE)
cm2 = confusionMatrix(test_set[,10], y_pred)
cm2

# ROC Curve KNN
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred),as.numeric(test_set$left))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - 1Knn")





