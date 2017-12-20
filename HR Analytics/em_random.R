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
training_set[-10] = scale(training_set[-10])
test_set[-10] = scale(test_set[-10])
#########
Out <- RPParallel(XTrain =training_set[,1:9], YTrain =training_set[,10], XTest =test_set[,1:9], d = 2, 
                  B1 = 10, B2 = 10, base = "QDA", projmethod = "Haar", estmethod = "training",  
                  splitsample = FALSE, k = seq(1, 25, by = 3), clustertype = "Default")
Out
#
library(mclust)
set.seed(13)
d_clust2 <- Mclust(as.matrix(Out), G=1:20)
plot(d_clust2)
#cluster
library(EMCluster)
emcluster=init.EM(Out, nclass = 6, EMC = .EMC,
                  min.n = NULL, min.n.iter = 5,
                  method = "Rnd.EM")
plotem(emcluster,Out)






