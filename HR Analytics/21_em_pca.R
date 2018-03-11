install.packages('scatterplot3d')
library(scatterplot3d)
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
dataset=na.omit(dataset)
#splitting the data
set.seed(123)
# Feature Scaling
dataset = scale(dataset[-10])
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
#emcluster
library(mclust)
set.seed(13)
d_clust2 <- Mclust(as.matrix(transformed), G=1:20)
plot(d_clust2)
#cluster
library(EMCluster)
emcluster=init.EM(transformed, nclass = 2, EMC = .EMC,
                  min.n = NULL, min.n.iter = 5,
                  method = "Rnd.EM")
plotem(emcluster,transformed)



