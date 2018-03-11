# load libraries
install.packages("mlbench")
library(mlbench)
library(caret)
# load the dataset
dataset = read.csv('hr_analytics.csv')
# Encoding the categorical variables as factors
dataset$sales = as.numeric(factor(dataset$sales,
                                  levels = c('accounting', 'hr', 'IT','management','marketing','product_mng','RandD','sales','support','technical'),
                                  labels = c(1, 2, 3,4,5,6,7,8,9,10)))
dataset$salary = as.numeric(factor(dataset$salary,
                                   levels = c('low', 'medium','high'),
                                   labels = c(1, 2,3)))

# Set the target variable as a factor
dataset$left <- as.numeric(dataset$left)
dataset=na.omit(dataset)
# summarize dataset
summary(dataset[,1:9])
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(dataset[,1:9], method=c("center", "scale", "ica"), n.comp=5)
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, dataset[,1:9])
# summarize the transformed dataset
summary(transformed)
transformed
#pair plot
pairs(transformed, col=rainbow(3)[dataset[,10]])
left=dataset[,10]
transformed2=cbind(transformed,left)
pairs(transformed[,1:5], main = "ICA",
      pch = 21, bg = c("red", "green3", "blue")[unclass(transformed2$left)])
