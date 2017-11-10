# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset=dataset[2:5]

# Encoding the categorical variables as factors
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Male', 'Female'),
                                   labels = c(0,1)))

# Set the target variable as a factor
dataset$Purchased <- as.factor(dataset$Purchased)
dataset$Gender <- as.numeric(dataset$Gender)
dataset$Age <- as.numeric(dataset$Age)
dataset$EstimatedSalary <- as.numeric(dataset$EstimatedSalary)
dataset=na.omit(dataset)
#splitting the data
#install.packages('h2o')
library(h2o)
h2o.init()

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = as.h2o(dataset), 
                         ratios = c(0.7, 0.15),  #partition data into 70%, 15%, 15% chunks
                         seed = 1)  #setting a seed will guarantee reproducibility
training_set <- splits[[1]]
validation_set <- splits[[2]]
test_set <- splits[[3]]

#grid search ANN
activation_opt <- c("Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout","Tanh","TanhWithDropout")
hidden=list(c(50,50),c(100,100),c(200,200))
hyper_params <- list(activation = activation_opt,
                     hidden=hidden)
grid_m <- h2o.grid("deeplearning", x = 1:3, y = 'Purchased',
                   grid_id = "grid2",
                   training_frame = as.h2o(training_set),
                   validation_frame=as.h2o(validation_set),
                   
                   seed = 1,
                   hyper_params = hyper_params,
                   standardize=TRUE)

gridperf_n <- h2o.getGrid(grid_id = "grid2", 
                          sort_by = "auc", 
                          decreasing = TRUE)
print(gridperf_n)



#bestmodel
best_model<- gridperf_n@model_ids[[1]]
best<- h2o.getModel(best_model)

#performance on new data
per= h2o.performance(best, newdata = as.h2o(test_set))
per

#model with best parameters Maxout hidden layers[100,100]
model = h2o.deeplearning(x=1:4,y = 'Purchased',
                         training_frame = as.h2o(training_set),
                         validation_frame = as.h2o(validation_set),
                         nfolds =10,
                         keep_cross_validation_predictions=TRUE,
                         standardize=TRUE,
                         activation = 'Maxout',
                         hidden = c(100,100),
                         #epochs = 100,
                         #train_samples_per_iteration = -2,
                         seed=1,
                         variable_importances = TRUE)


h2o.auc(model)
#variable importance
h2o.varimp(model)
h2o.varimp_plot(model)

# Predicting the Test set results
y_pred = h2o.predict(model, newdata = as.h2o(test_set[-4]))
y_pred.R=as.data.frame(y_pred)
test_set.R=as.data.frame(test_set)
head(y_pred.R)

cm_d=confusionMatrix(y_pred.R[,1],test_set.R[,4])
cm_d

#ROC
plot(per, type = "roc")
h2o.auc(best,valid=TRUE)#on test
h2o.auc(best,valid=FALSE)#on train
#auc vs duration
plot(best, timestep = "duration", metric = "auc")
plot(best, timestep = "duration", metric = "classification_error")
plot(best, timestep = "duration", metric = "logloss")
plot(best, timestep = "duration", metric = "rmse")

#plotting activation vs accuracy 
a=c("Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout","Tanh","TanhWithDropout")
a1=rep(0,6)
for(i in 1:6)
{
  model1 = h2o.deeplearning(x=1:4,y = 'Purchased',
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
plot(a1,type="o",xlab="activation",ylab="Accuracy",main="ads-Ann")

#hidden layers vs accuracy plot
b=c(c(10,10),c(25,25),c(50,50),c(100,100),c(200,200),c(250,250),c(300,300),c(350,350))
a2=rep(0,8)
for(i in 1:8)
{
  model1 = h2o.deeplearning(x=1:4,y = 'Purchased',
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

plot(a2,type="o",xlab="hidden layers",ylab="Accuracy",main="Ads_ANN")




