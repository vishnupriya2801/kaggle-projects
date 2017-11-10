getwd()
setwd('/Users/vishnupriyakodavatiganti/Desktop/MLP2/ads')

#setwd("/Users/vishnupriyakodavatiganti/Desktop/hr_analytics")
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
dataset = read.csv('Social_Network_Ads.csv')
dataset=dataset[2:5]

# Encoding the categorical variables as factors
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Male', 'Female'),
                                   labels = c(0,1)))

# Set the target variable as a factor
dataset$Purchased <- as.factor(dataset$Purchased)
dataset$Gender <- as.numeric(dataset$Gender)
dataset$Age <- as.numeric(dataset$Age)
dataset$EstimatedSalary <- as.numeric(dataset$EstimatedSalary)
dataset=na.omit(dataset)
#splitting the data
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#sapply(dataset,class)

# Feature Scaling
training_set[-4] = scale(training_set[-4])
test_set[-4] = scale(test_set[-4])

#install.packages('class')
library(class)
train=training_set[,-4]
test=test_set[,-4]
cl=training_set[,4]
#cross validation
y_pred_cv=knn.cv(train,cl,k=3,prob =FALSE)
cm = confusionMatrix(cl, y_pred_cv)
cm

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
plot(accuracy,type="o",xlab="Number of neighbours",ylab="Accuracy",main="Ads-Knn")

#prediction
y_pred=knn(train,test,cl,k=3,prob=TRUE)
cm2 = confusionMatrix(test_set[,4], y_pred)
cm2

# ROC Curve KNN
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred),as.numeric(test_set$Purchased))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - 3Knn")

#prediction
y_pred_1=knn(train,test,cl,k=6,prob=TRUE)
cm2 = confusionMatrix(test_set[,4], y_pred)
cm2

# ROC Curve KNN
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred_1),as.numeric(test_set$Purchased))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - 6Knn")


