library(xgboost)
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
dataset = read.csv('iris.csv')


#  Encoding the categorical variables as factors
dataset$class = as.numeric(factor(dataset$class,
                                  levels = c('Iris-setosa', 'Iris-versicolor','Iris-virginica'),
                                  labels = c(1, 2,3)))

#splitting the data
set.seed(123)
split = sample.split(dataset$class, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Kernel SVM
# Fitting Kernel SVM to the Training set
#linear kernel
# install.packages('e1071')
library(e1071)
classifier.l = svm(formula = class ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
# Predicting the Test set results
y_pred.l = predict(classifier.l, newdata = test_set[-5])
# Making the Confusion Matrix
cm.l = confusionMatrix(test_set[, 5], y_pred.l)
cm.l

#Roc graph
install.packages('pROC')
library(pROC)
y_pred.l=as.numeric(y_pred.l)
multiclass.roc(as.numeric(test_set$class), y_pred.l)

#radial kernel
# install.packages('e1071')
library(e1071)
classifier.r = svm(formula = class ~ .,
                   data = training_set,
                   type = 'C-classification',
                   kernel = 'radial')
# Predicting the Test set results
y_pred.r = predict(classifier.r, newdata = test_set[-5])
# Making the Confusion Matrix
cm.r = confusionMatrix(test_set[, 5], y_pred.r)
cm.r
#Roc graph
install.packages('pROC')
library(pROC)
y_pred.r=as.numeric(y_pred.r)
multiclass.roc(as.numeric(test_set$class), y_pred.r)

#polynomial kernel
# install.packages('e1071')
library(e1071)
classifier.p = svm(formula = class ~ .,
                   data = training_set,
                   type = 'C-classification',
                   kernel = 'polynomial',
                   degree = 2,
                   gamma = 1)
# Predicting the Test set results
y_pred.p = predict(classifier.p, newdata = test_set[-5])
# Making the Confusion Matrix
cm.p = confusionMatrix(test_set[, 5], y_pred.p)
cm.p
#auc
library(pROC)
y_pred.p=as.numeric(y_pred.p)
multiclass.roc(as.numeric(test_set$class), y_pred.p)


#sigmoid kernel
# install.packages('e1071')
library(e1071)
classifier.s = svm(formula = class ~ .,
                   data = training_set,
                   type = 'C-classification',
                   kernel = 'sigmoid',
                   gamma = 2)
# Predicting the Test set results
y_pred.s = predict(classifier.s, newdata = test_set[-5])
# Making the Confusion Matrix
cm.s = confusionMatrix(test_set[, 5], y_pred.s)
cm.s
#ROC curve
library(pROC)
y_pred.s=as.numeric(y_pred.s)
multiclass.roc(as.numeric(test_set$class), y_pred.s)


