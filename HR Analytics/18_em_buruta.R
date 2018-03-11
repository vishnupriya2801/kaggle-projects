install.packages("Boruta")
library(Boruta)

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
dataset[-10] = scale(dataset[-10])

# Decide if a variable is important or not using Boruta
#runs randon forest in the background
boruta_output <- Boruta(dataset$left ~ ., data=na.omit(dataset[,-10]), doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance


# Set the target variable as a factor
data=dataset[,-c(6,7,8,10)]

# Feature Scaling
data= scale(data)
######
library(mclust)
set.seed(13)
d_clust2 <- Mclust(as.matrix(data), G=1:20)
plot(d_clust2)
#########
emcluster=init.EM(data, nclass = 5, EMC = .EMC,
                  min.n = NULL, min.n.iter = 5,
                  method = c("em.EM", "Rnd.EM"))

plotem(emcluster,data)

