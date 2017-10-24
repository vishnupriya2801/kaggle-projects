install.packages('xgboost')
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

sapply(dataset,class)
dataset$sepa_length=as.numeric(dataset$sepa_length)
dataset$sepal_width=as.numeric(dataset$sepal_width)
dataset$petal_length=as.numeric(dataset$petal_length)
dataset$petal_width=as.numeric(dataset$petal_width)
dataset$class=as.factor(dataset$class)
#splitting the data
set.seed(123)
split = sample.split(dataset$class, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Fitting XGBoost to the Training set
install.packages('xgboost')
library(xgboost)
classifier = xgboost(data = as.matrix(training_set[-5]), label = training_set$class, nrounds = 10)


# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(test_set[-5]))
y_pred=round(y_pred)
y_pred=as.matrix(y_pred)
y_pred =y_pred +0
#subset=training_set[,-5]

# Making the Confusion Matrix
library(caret)
cm = confusionMatrix(test_set[, 5], y_pred,mode='everything')
cm

library(pROC)
y_pred=as.numeric(y_pred)
multiclass.roc(as.numeric(test_set$class), y_pred)

# With pruning - changing gamma
library(xgboost)
classifier2 = xgboost(data = as.matrix(training_set[-5]), label = training_set$class,params=list(gamma=0.65),nrounds = 10)

# Predicting the Test set results after prune
y_pred2 = predict(classifier2, newdata = as.matrix(test_set[-5]))
y_pred2 = round(y_pred)
y_pred2=as.matrix(y_pred2)
y_pred2 =y_pred2 +0

# Making the Confusion Matrix
cm2 = confusionMatrix(test_set[, 5], y_pred2)
cm2

library(pROC)
y_pred2=as.numeric(y_pred2)
multiclass.roc(as.numeric(test_set$class), y_pred2)

