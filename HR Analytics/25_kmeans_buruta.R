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
dataset=na.omit(dataset)
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
kmeans = kmeans(x = data, centers =4,nstart=25 )
fviz_cluster(kmeans, data = data)

