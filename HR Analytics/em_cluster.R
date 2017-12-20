getwd()
setwd('/Users/vishnupriyakodavatiganti/Desktop/MLP4/hr')

install.packages('EMCluster')
install.packages('caret')
install.packages('caTools')
install.packages('FastICA')
install.packages('factoextra')

library(caTools)
library(EMCluster)
library(factoextra)
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
data=dataset[,-10]
#dataset$left <- as.factor(dataset$left)
data=na.omit(data)

# Feature Scaling
data= scale(data)

library(mclust)
set.seed(13)
d_clust2 <- Mclust(as.matrix(data), G=1:20)
plot(d_clust2)

emcluster=init.EM(data, nclass = 2, EMC = .EMC,
                  min.n = NULL, min.n.iter = 5,
                  method = "Rnd.EM")

plotem(emcluster,data)
##########


