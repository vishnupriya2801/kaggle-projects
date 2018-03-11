getwd()
setwd("/Users/vishnupriyakodavatiganti/Desktop/MLP4/hr")
install.packages('h2o')
install.packages('scatterplot3d')
library(h2o)
library(caret)
# Importing the dataset
dataset = read.csv('hr_analytics.csv')

#unique(dataset[,10], incomparables = FALSE)
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
#splitting the data
set.seed(123)
# Feature Scaling
dataset[-10] = scale(dataset[-10])
#####working
# load the libraries
library(mlbench)
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(dataset, method=c("center", "scale", "pca"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, dataset)
# summarize the transformed dataset
summary(transformed)
#plot first 2 components
library(lattice)
pca.plot <- xyplot(transformed[,1] ~ transformed[,2])
pca.plot$xlab <- "First Component"
pca.plot$ylab <- "Second Component"
pca.plot

#scatterplot3d
library(scatterplot3d)
scatterplot3d(transformed[,1], transformed[,2], transformed[,3], highlight.3d=TRUE,
              col.axis="blue", col.grid="lightblue",
              main="scatterplot3d - 2", pch=20)

