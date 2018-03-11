install.packages("RPEnsemble")
library(RPEnsemble)
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
dataset=na.omit(dataset)
#splitting the data
set.seed(123)
split = sample.split(dataset$left, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
# Feature Scaling
dataset[-10] = scale(dataset[-10])
#########
Out <- RPParallel(XTrain =training_set[,1:9], YTrain =training_set[,10], XTest =test_set[,1:9], d = 2, 
                  B1 = 10, B2 = 10, base = "QDA", projmethod = "Haar", estmethod = "training",  
                  splitsample = FALSE, k = seq(1, 25, by = 3), clustertype = "Default")
Out


# Using the elbow method to find the optimal number of clusters
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(Out, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = Out, centers =4,nstart=25 )
fviz_cluster(kmeans, data = Out)





