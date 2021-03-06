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
dataset$left <- as.factor(dataset$left)
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
transformed
transformed=na.omit(transformed)
left=dataset[,10]
transformed=cbind(transformed,left)

library(h2o)
h2o.init()
# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = as.h2o(transformed), 
                         ratios = c(0.7, 0.15),  #partition data into 70%, 15%, 15% chunks
                         seed = 1)  #setting a seed will guarantee reproducibility
training_set <- splits[[1]]
validation_set <- splits[[2]]
test_set <- splits[[3]]

#grid search ANN
activation_opt <- c("Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout","Tanh","TanhWithDropout")
hidden=list(c(50,50),c(100,100),c(200,200),c(300,300))
hyper_params <- list(activation = activation_opt,
                     hidden=hidden)
grid_m <- h2o.grid("deeplearning", x = 1:5, y = 'left',
                   grid_id = "grid_n21",
                   training_frame = as.h2o(training_set),
                   validation_frame=as.h2o(validation_set),
                   
                   seed = 1,
                   hyper_params = hyper_params,
                   standardize=TRUE)

gridperf_n <- h2o.getGrid(grid_id = "grid_n21", 
                          sort_by = "auc", 
                          decreasing = TRUE)
print(gridperf_n)

#bestmodel
best_model<- gridperf_n@model_ids[[1]]
best<- h2o.getModel(best_model)
best

#performance on new data
per= h2o.performance(best, newdata = as.h2o(test_set))
per

#model with best parameters maxout hidden layers[50,50]
model = h2o.deeplearning(x=1:5,y = 'left',
                         training_frame = as.h2o(training_set),
                         validation_frame = as.h2o(validation_set),
                         nfolds =10,
                         keep_cross_validation_predictions=TRUE,
                         standardize=TRUE,
                         activation = 'Maxout',
                         hidden = c(50,50),
                         #epochs = 100,
                         #train_samples_per_iteration = -2,
                         seed=1,
                         variable_importances = TRUE)

#variable importance
h2o.varimp(model)
h2o.varimp_plot(model)

# Predicting the Test set results
y_pred = h2o.predict(model, newdata = as.h2o(test_set[-6]))
y_pred.R=as.data.frame(y_pred)
test_set.R=as.data.frame(test_set)
head(y_pred.R)

cm_d=confusionMatrix(y_pred.R[,1],test_set.R[,6])
cm_d

#ROC
plot(per, type = "roc")
h2o.auc(best,valid=TRUE)#on test
h2o.auc(best,valid=FALSE)#on train
#auc vs duration
plot(best, timestep = "duration", metric = "rmse")
plot(best, timestep = "duration", metric = "classification_error")
plot(best, timestep = "duration", metric = "logloss")
plot(best, timestep = "duration", metric = "auc")


#plotting activation vs accuracy 
a=c("Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout","Tanh","TanhWithDropout")
a1=rep(0,6)
for(i in 1:6)
{
  model1 = h2o.deeplearning(x=1:5,y = 'left',
                            training_frame = as.h2o(training_set),
                            validation_frame = as.h2o(validation_set),
                            nfolds =10,
                            keep_cross_validation_predictions=TRUE,
                            standardize=TRUE,
                            activation =a[i],
                            hidden =c(50,50),
                            #epochs = 100,
                            #train_samples_per_iteration = -2,
                            seed=1,
                            variable_importances = TRUE) 
  
  print(a[i])
  a1[i]=h2o.auc(model1)
}
a1
plot(a1,type="o",xlab="activation",ylab="Accuracy",main="hr_analytics-Ann")

#hidden layers vs accuracy plot
b=c(c(10,10),c(25,25),c(50,50),c(100,100),c(200,200),c(250,250),c(300,300),c(350,350))
a2=rep(0,8)
for(i in 1:8)
{
  model1 = h2o.deeplearning(x=1:5,y = 'left',
                            training_frame = as.h2o(training_set),
                            validation_frame = as.h2o(validation_set),
                            nfolds =10,
                            keep_cross_validation_predictions=TRUE,
                            standardize=TRUE,
                            activation ="Maxout",
                            hidden =b[i],
                            #epochs = 100,
                            #train_samples_per_iteration = -2,
                            seed=1,
                            variable_importances = TRUE) 
  print(b[i])  
  a2[i]=h2o.auc(model1)
}
a2
plot(a2,type="o",xlab="hidden layers",ylab="Accuracy",main="hr_analytics_ANN")






