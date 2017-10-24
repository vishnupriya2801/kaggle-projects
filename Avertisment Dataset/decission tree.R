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

# Decision Tree Classifier
classifier <- rpart(formula = class~.,
                    data = training_set,method = "class",
                    parms = list(split="gini"))

# Decision Tree plot
fancyRpartPlot(classifier,cex=0.6)
# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-5],type="class")
# Confusion Matrix
confusionMatrix(test_set$class, y_pred)

#Roc graph
install.packages('pROC')
library(pROC)
y_pred=as.numeric(y_pred)
multiclass.roc(as.numeric(test_set$class), y_pred)
#auc(as.numeric(test_set$class), as.numeric(y_pred))

# Pruning the tree
dt1<-prune(classifier,cp=0.025)
rpart.plot(dt1,main = "Classification Tree for Predicting Purchases", cex=0.7)
asRules(dt1)
pred<- predict(dt1,newdata = test_set,type="class")
confusionMatrix(pred,test_set$class)

pred=as.numeric(pred)
multiclass.roc(as.numeric(test_set$class), pred)
