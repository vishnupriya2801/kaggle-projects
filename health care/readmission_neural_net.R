# Set the target variable as a factor
dataset2$READMISSION <- as.factor(dataset2$READMISSION)

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
model = h2o.deeplearning(x=2:938,y = 'READMISSION',
                         training_frame = as.h2o(training_set),
                         validation_frame = as.h2o(validation_set),
                         nfolds =10,
                         keep_cross_validation_predictions=TRUE,
                         standardize=TRUE,
                         activation = 'Tanh',
                         hidden = c(100,100),
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
data<-dataset2[2:938]
col<-which(colnames(data) %in% top)
ndata<-data[,sort(col)]
ndata<-cbind(ndata,dataset2[,1])
ndata<-ndata[1:130]

#adding the target variable to the subset data
colnames(ndata)[131]<-"READMISSION"
colnames(ndata)

#grid search ANN to find best parameters
activation_opt <- c("Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout","Tanh","TanhWithDropout")
hidden=list(c(100,100),c(200,200),c(250,200),c(300,300))
hyper_params <- list(activation = activation_opt,
                     hidden=hidden)
grid_m <- h2o.grid("deeplearning", x = 1:130, y = 'READMISSION',
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

#modelling with best parameter values
model2 = h2o.deeplearning(x=2:938,y = 'READMISSION',
                         training_frame = as.h2o(training_set),
                         validation_frame = as.h2o(validation_set),
                         nfolds =10,
                         keep_cross_validation_predictions=TRUE,
                         standardize=TRUE,
                         activation = "RectifierWithDropout",
                         hidden = c(250,200),
                         epochs = 100,
                         train_samples_per_iteration = -2,
                         seed=32)
                        
# Predicting the Test set results
y_pred = h2o.predict(model2, newdata = as.h2o(test_set[-131]))
y_pred.R=as.data.frame(y_pred)
test_set.R=as.data.frame(test_set)
head(y_pred.R)

cm_d=confusionMatrix(y_pred.R[,1],test_set.R[,131])
cm_d

#ROC
plot(per, type = "roc")
h2o.auc(best,valid=TRUE)#on test
h2o.auc(best,valid=FALSE)#on train
