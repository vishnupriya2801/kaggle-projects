# Importing the dataset
dataset = read.csv('hr_analytics.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$left, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
library(caret)

# Fitting SVM to the Training set
#install.packages('e1071')
library(e1071)

#linear kernel
classifier.l = svm(formula = left ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred.l = predict(classifier.l, newdata = test_set[-10])
# Making the Confusion Matrix
confusionMatrix(test_set[, 10], y_pred.l)

#Plotting RoC Curve 
pred1 <- prediction(as.numeric(y_pred.l),as.numeric(test_set$left))
perf<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred.l),as.numeric(test_set$left))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - Pruned Decision Tree")


#radial kernel
classifier.r = svm(formula = left ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred.r = predict(classifier.r, newdata = test_set[-10])
# Making the Confusion Matrix
confusionMatrix(test_set[, 10], y_pred.r)

#Plotting RoC Curve 
pred1 <- prediction(as.numeric(y_pred.r),as.numeric(test_set$left))
perf<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred.r),as.numeric(test_set$left))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve - Pruned Decision Tree")

#polynomial kernel
classifier.p = svm(formula = left ~ .,
                   data = training_set,
                   type = 'C-classification',
                   kernel = 'polynomial',
                   degree=3, 
                   gamma =1 )

# Predicting the Test set results
y_pred.p = predict(classifier.p, newdata = test_set[-10])
# Making the Confusion Matrix
confusionMatrix(test_set[, 10], y_pred.p)

#Plotting RoC Curve 
pred1 <- prediction(as.numeric(y_pred.p),as.numeric(test_set$left))
perf<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred.p),as.numeric(test_set$left))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve ")

#sigmoid kernel
classifier.s = svm(formula = left ~ .,
                   data = training_set,
                   type = 'C-classification',
                   kernel = 'sigmoid',
                   gamma =1 )

# Predicting the Test set results
y_pred.s = predict(classifier.s, newdata = test_set[-10])
# Making the Confusion Matrix
confusionMatrix(test_set[, 10], y_pred.s)

#Plotting RoC Curve 
pred1 <- prediction(as.numeric(y_pred.s),as.numeric(test_set$left))
perf<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
par(mfrow=c(1,1))
ROCRpred = prediction(as.numeric(y_pred.s),as.numeric(test_set$left))
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc <- slot(performance(ROCRpred, "auc"), "y.values")[[1]] # Area Under Curve
plot(ROCRperf, colorize=TRUE)
abline(h=seq(0,1,0.05), v=seq(0,1,0.05), col = "lightgray", lty = "dotted")
lines(c(0,1),c(0,1), col = "gray", lwd =2)
text(0.6,0.2,paste("AUC=", round(auc,4), sep=""), cex=1.4)
title("ROC Curve ")

