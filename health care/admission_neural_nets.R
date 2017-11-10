#install required packages
library(lattice)
library(ggplot2)
library(caret)
library(caTools)
library(h2o)

# Importing the dataset
dataset = read.csv('healthcare.csv')

#changing the problem to binary since admission and readmission have number of times patient was admitted/readmitted
for (i in 1:nrow(dataset)){
  
  if(dataset[i,1]>0)
  {
    dataset[i,1]<-1
  }

}
dataset[5284,2]


for(i in 1:nrow(dataset)){
  
  if(dataset[i,2] > 0)
  {
    dataset[i,2]<-1
  }
}


# Encoding the categorical variables as factors
dataset$SEX = as.numeric(factor(dataset$SEX,
                                   levels = c('M', 'F'),
                                   labels = c(0,1)))

dataset$ESRD_IND = as.numeric(factor(dataset$ESRD_IND,
                                   levels = c('N', 'Y'),
                                   labels = c(0,1)))

dataset$HOSPICE_IND = as.numeric(factor(dataset$HOSPICE_IND,
                                     levels = c('N', 'Y'),
                                   labels = c(0,1)))


dataset$MAJOR_GEOGRAPHY = as.numeric(factor(dataset$MAJOR_GEOGRAPHY,
                                           levels = c('Central', 'Florida','Northern','Western','Southeastern'),
                                           labels = c(0,1,2,3,4)))

dataset$MINOR_GEOGRAPHY = as.numeric(factor(dataset$MINOR_GEOGRAPHY,
                                            levels = c('Central', 'California','Central West','East','East Central','FL Non HMO','Gulf States','Internmountain','Mid-Atlantic','Mid-South','North Central','North Florida','South Florida','Northeast','Pacific','Southeast: GA-SC','Texas'),
                                            labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)))

#omiiting null values
dataset=na.omit(dataset)

splitting data set into two problems admission and re admission
dataset1=dataset[,-c(2)]
dataset2=dataset[,-c(1)]

# Set the target variable as a factor
dataset1$ADMISSIONS <- as.factor(dataset$ADMISSION)

#calling h2o package
h2o.init()

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = as.h2o(ndata), 
                         ratios = c(0.7, 0.15),  #partition data into 70%, 15%, 15% chunks
                         seed = 32)  #setting a seed will guarantee reproducibility
                         
training_set<- splits[[1]]
validation_set <- splits[[2]]
test_set <- splits[[3]]

#modelling with random parameter values to find variable importances
model = h2o.deeplearning(x=2:938,y = 'ADMISSION',
                         training_frame = as.h2o(training_set),
                         validation_frame = as.h2o(validation_set),
                         nfolds =10,
                         keep_cross_validation_predictions=TRUE,
                         standardize=TRUE,
                         activation = 'Maxout',
                         hidden = c(200,200),
                         epochs = 100,
                         train_samples_per_iteration = -2,
                         seed=32,
                         variable_importances = TRUE)
                        
                        
#variable importance
imp<-h2o.varimp(model)
h2o.varimp_plot(model)
imp

#finding the most important variables
top<-rep(0,884)
i=1
j=1
for(i in 1:884){
  if (imp[i,2] >mean(imp[,2]) ){
    topq[j]<-imp[i,1]
    j=j+1
  }
  i=i+1
  }

#subsetting the dataset with the names in the top list
data<-dataset1[2:938]
col<-which(colnames(data) %in% top)
ndata<-data[,sort(col)]
ndata<-cbind(ndata,dataset1[,1])
ndata<-ndata[1:130]

#adding the target variable to the subset data
colnames(ndata)[131]<-"ADMISSION"
colnames(ndata)

#grid search ANN to find best parameters
activation_opt <- c("Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout","Tanh","TanhWithDropout")
hidden=list(c(50,50),c(100,100),c(200,200),c(300,300))
hyper_params <- list(activation = activation_opt,
                     hidden=hidden)
grid_m <- h2o.grid("deeplearning", x = 1:130, y = 'ADMISSION',
                   grid_id = "grid1",
                   training_frame = as.h2o(training_set),
                   validation_frame=as.h2o(validation_set),
                   
                   seed = 1,
                   hyper_params = hyper_params,
                   standardize=TRUE)

gridperf_n <- h2o.getGrid(grid_id = "grid_n", 
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

#auc,confusion matrix
h2o.auc(best)
h2o.confsuionMatrix(best)

#alternatively predicting and finding Roc
# Predicting the Test set results
y_pred = h2o.predict(model, newdata = as.h2o(test_set[-131]))
y_pred.R=as.data.frame(y_pred)
test_set.R=as.data.frame(test_set)
head(y_pred.R)

cm_d=confusionMatrix(y_pred.R[,1],test_set.R[,131])
cm_d

#ROC
plot(per, type = "roc")
h2o.auc(best,valid=TRUE)#on test
h2o.auc(best,valid=FALSE)#on train

