getwd()
setwd('/Users/vishnupriyakodavatiganti/Desktop/MLP4/hr')

install.packages('caTools')
install.packages('cluster')
install.packages('factoextra')

library(caTools)
library(cluster)
library(factoextra)
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
data=dataset[,-10]
#dataset$left <- as.factor(dataset$left)
data=na.omit(data)

# Feature Scaling
data= scale(data)

# Using the elbow method to find the optimal number of clusters
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(data, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = data, centers =4,nstart=25 )
kmeans
fviz_cluster(kmeans, data = data)

