library(caret)
# Importing the dataset
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
#splitting the data
set.seed(123)
# Feature Scaling
dataset = scale(dataset[-10])
#pca
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
kmeans = kmeans(x = transformed, centers =5,nstart=25 )
fviz_cluster(kmeans, data = transformed)

#scatterplot3d
library(scatterplot3d)
scatterplot3d(transformed[,1], transformed[,2], transformed[,3], highlight.3d=TRUE,
              col.axis="blue", col.grid="lightblue",
              main="scatterplot3d - 2", pch=20)

