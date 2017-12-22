#setwd("C:/AML - BUAN 6341'")

getwd()
setwd('/Users/vishnupriyakodavatiganti/Desktop/MLP2/hr_analytics')
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
dataset = read.csv('hr_analytics.csv')

# Encoding the categorical variables as factors
dataset$sales = as.numeric(factor(dataset$sales,
                                  levels = c('accounting', 'hr', 'it','management','marketing','product_mng','RanD','sales','support','technical'),
                                  labels = c(1, 2, 3,4,5,6,7,8,9,10)))
dataset$salary = as.numeric(factor(dataset$salary,
                                   levels = c('low', 'medium','high'),
                                   labels = c(1, 2,3)))



#correlation plot
M <- cor(dataset[,-c(8:9)])
corrplot(M, method="color")

#box plots
ggplot(dataset, aes(x =satisfaction_level, y = time_spend_company, fill = factor(left), colour = factor(left))) + 
  geom_boxplot(outlier.colour = "black") + xlab(" satisfaction_level") + ylab(" time_spend") 


#exploration on people who left the company
hr_hist <- subset.data.frame(dataset,dataset$left==1)
nrow(hr_hist)
histogram(dataset$left,dataset)

par(mfrow=c(1,3))
histogram(hr_hist$average_montly_hours,hr_hist,main = " average monthly hours") 
hist(hr_hist$last_evaluation, main = "Last evaluation")
hist(hr_hist$salary,col="#3090C7", main = "salary")
hist(hr_hist$satisfaction_level,col="#3090C7", main = "satisfaction_level")
# good people who are leaving
hr_good_leaving_people <- subset.data.frame(hr_hist,hr_hist$last_evaluation >= 0.70 | hr_hist$time_spend_company >= 4 | hr_hist$number_project > 5)
nrow(hr_good_leaving_people)

#why good people leave 
hr_good_leaving_people2 <-subset.data.frame(hr_hist, last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
hr_good_people_select <- hr_good_leaving_people2[,c(1,3,7)]
M <- cor(hr_good_people_select)
corrplot(M, method="circle")


# summary of variables in good people leaving
summary(hr_good_leaving_people)

#splitting the data
set.seed(123)
split = sample.split(dataset$left, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)




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
# Set the target variable as a factor
dataset$left <- as.factor(dataset$left)
sapply(dataset,class)
summary(dataset)
