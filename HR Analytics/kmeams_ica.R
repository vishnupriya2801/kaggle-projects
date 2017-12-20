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

# Using the elbow method to find the optimal number of clusters
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(transformed, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x =transformed, centers =4,nstart=25 )
fviz_cluster(kmeans, data = transformed)
