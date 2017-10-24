setwd('/Users/vishnupriyakodavatiganti/Desktop/a')

install.packages('caTools')
install.packages('caret')
install.packages('lattice')
install.packages('ggplot2')
install.packages('e1071')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('corrplot')
install.packages('dplyr')
install.packages('RColorBrewer')
install.packages('rattle')
install.packages('ROCR')
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


# Encoding the categorical variables as factors
dataset$class = as.numeric(factor(dataset$class,
                                   levels = c('Iris-setosa', 'Iris-versicolor','Iris-virginica'),
                                   labels = c(1, 2,3)))

# Encoding features as numerics


summary(dataset)

#correlation plot
library(corrplot)
M <- cor(dataset[,-c(8:9)])
corrplot(M, method="color")

#exploration on Purchased target variable
#iris <- subset.data.frame(dataset,dataset$Purchased==1)
#nrow(ad_hist)
#histogram(dataset$Purchased,dataset)

#par(mfrow=c(1,3))
#histogram(ad_hist$Age,ad_hist,main = " Age") 
#hist(ad_hist$EstimatedSalary, main = "Estimated Salary")
#hist(ad_hist$Age,col="#3090C7", main = "Age")

