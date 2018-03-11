rm(list=ls())
library(glmnet)
library(randomForest)
library(caret)
library(e1071)
library(DMwR)
# read data
data = read.csv('tmdb_5000_movies_step2_with_keyword.csv')
data = data[-which(data$revenue_budget_ratio > 100),]
#head(data)
#colnames(data)

Y <- data$vote_average
X <- data[, -c(2, 3, 5, 1887)] #with keyword
Y <- as.numeric(Y > 7)

# odd id as train, even id as test
X_train <- X[data$id %% 2 == 1, ]
X_test <- X[data$id %% 2 == 0, ]
Y_train <- Y[data$id %% 2 == 1]
Y_test <- Y[data$id %% 2 == 0]

# select the train x whose correlation with y is not less than 0.1
X_test <- X_test[,colSums(X_train) > 1]
X_train <- X_train[,colSums(X_train) > 1]
r <- abs(cor(Y_train, X_train))
X_train <- X_train[, r >= 0.05]
X_test <- X_test[, r >= 0.05]

# SMOTE
a = as.factor(Y_train)
b = SMOTE(a~.,cbind(X_train,a), perc.over = 200, perc.under = 150)
X_train_SMOTE = b[,-204]
Y_train_SMOTE = as.numeric(paste(b[,c('a')]))

# Under sampling
Y_train1 <- Y_train[Y_train == 1]
X_train1 <- X_train[Y_train == 1,]
Y_train0 <- Y_train[Y_train == 0]
X_train0 <- X_train[Y_train == 0,]
set.seed(0)
temp <- sample(seq(1,length(Y_train0),1), length(Y_train1))
Y_train_under <- as.factor(c(Y_train1, Y_train0[temp]))
X_train_under <- rbind(X_train1, X_train0[temp,])

# Over sampling
Y_train1 <- Y_train[Y_train == 1]
X_train1 <- X_train[Y_train == 1,]
Y_train0 <- Y_train[Y_train == 0]
X_train0 <- X_train[Y_train == 0,]
set.seed(0)
temp <- sample(length(Y_train1), length(Y_train0), replace = T)
Y_train_over <- as.factor(c(Y_train0, Y_train1[temp]))
X_train_over <- rbind(X_train0, X_train1[temp,])

criterion <- function(y,pred_y){
  TP = sum(y[pred_y==1]==1)
  TN = sum(y[pred_y== 0]== 0)
  FP = sum(y[pred_y==1]== 0)
  FN = sum(y[pred_y==0]==1)
  sensitivity = TP/(TP+FN)
  specificity = TN/(TN+FP)
  acc = (TP+TN)/(TP+TN+FP+FN)
  precision = TP/(TN+FP)
  F1 = 2 * TP / ((2 * TP) + FP + FN)
  return(list(sensitivity=sensitivity,specificity=specificity,acc=acc,precision=precision,F1=F1))
}

# lasso
lasso <- function(X_train,Y_train,X_test,Y_test){
  set.seed(0)
  X_train <- as.matrix(X_train)
  X_test <- as.matrix(X_test)
  Y_train <- as.factor(Y_train)
  Y_test <- as.factor(Y_test)
  lasso2.cv <- cv.glmnet(X_train, Y_train, alpha = 1, nfolds = 10, type.measure="auc", family="binomial", type.multinomial = 'grouped')
  lasso2 <- glmnet(X_train, Y_train, alpha = 1, lambda = lasso2.cv$lambda.min,family="binomial")
  Y_pre_lasso1 <- predict(lasso2, X_test, type = "class")
}
lasso_under_pre = lasso(X_train_under,Y_train_under,X_test,Y_test)
lasso_over_pre = lasso(X_train_over,Y_train_over,X_test,Y_test)
lasso_SMOTE_pre = lasso(X_train_SMOTE,Y_train_SMOTE,X_test,Y_test)
lasso_raw_pre = lasso(X_train,Y_train,X_test,Y_test)
lasso_under = criterion(Y_test,lasso_under_pre)
lasso_over = criterion(Y_test,lasso_over_pre)
lasso_SMOTE = criterion(Y_test,lasso_SMOTE_pre)
lasso_raw= criterion(Y_test,lasso_raw_pre)

#logistic regession
logistic <- function(X_train,Y_train,X_test,Y_test){
  set.seed(0)
  Y_train = as.factor(Y_train)
  logistic = glm(Y_train~.,data=cbind(X_train,Y_train),family = "binomial")
  log_pred_y = predict(logistic,X_test)
  log_pred_y -> log_pred_label
  log_pred_label[log_pred_label>0.5] = 1
  log_pred_label[log_pred_label<=0.5] = 0
  return(log_pred_label)
}
logistic_under_pre = logistic(X_train_under,Y_train_under,X_test,Y_test)
logistic_over_pre = logistic(X_train_over,Y_train_over,X_test,Y_test)
logistic_SMOTE_pre = logistic(X_train_SMOTE,Y_train_SMOTE,X_test,Y_test)
logistic_raw_pre = logistic(X_train,Y_train,X_test,Y_test)
logistic_under = criterion(Y_test,logistic_under_pre)
logistic_over = criterion(Y_test,logistic_over_pre)
logistic_SMOTE = criterion(Y_test,logistic_SMOTE_pre)
logistic_raw= criterion(Y_test,logistic_raw_pre)


svm542 <-function(X_train,Y_train,X_test,Y_test,kernel,cost){
  set.seed(0)
  Y_train = as.factor(Y_train)
  s = svm(Y_train~.,data=cbind(X_train,Y_train),family = "binomial",kernel=kernel,cost=cost)
  svm_pred_y = predict(s,X_test)
  return(svm_pred_y)
}
svm542_tune <- function(X_train,Y_train){
  parameter = c(0.01,0.1,1,10,100)
  kernel = c("linear","polynomial","radial")
  svm_tume = matrix(0,15,3)
  colnames(svm_tume) = c('cost','kernel','sensitivity')
  p = 0
  for (i in 1:length(parameter)){
    for (j in 1:length(kernel)){
      p = p + 1
      cv_partition <- createDataPartition(Y_train, p=.9, list=FALSE)
      train_1_x = X_train[cv_partition,]
      train_validation_x = X_train[-cv_partition,]
      train_1_y = Y_train[cv_partition]
      train_validation_y = Y_train[-cv_partition]
      svm_pred_y = svm542(train_1_x,train_1_y,train_validation_x,train_validation_y,kernel=kernel[j],cost=parameter[i])
      svm_acc = criterion(train_validation_y,svm_pred_y)
      svm_tume[p,1] = parameter[i];svm_tume[p,2] = kernel[j];
      svm_tume[p,3] = svm_acc$sensitivity
    }
  }
  return(svm_tume)
}
svm_under_pre = svm542_tune(X_train_under,Y_train_under)
svm_over_pre = svm542_tune(X_train_over,Y_train_over)
svm_SMOTE_pre = svm542_tune(X_train_SMOTE,Y_train_SMOTE)
save(svm_under_pre,svm_over_pre,svm_SMOTE_pre,file='SVM_tume.Rdata')

svm_under_pre = svm542(X_train_under,Y_train_under,X_test,Y_test,'linear',1)
svm_over_pre = svm542(X_train_over,Y_train_over,X_test,Y_test,'linear',1)
svm_SMOTE_pre = svm542(X_train_SMOTE,Y_train_SMOTE,X_test,Y_test,'linear',1)
svm_raw_pre = svm542(X_train,Y_train,X_test,Y_test,'linear',1)
svm_under = criterion(Y_test,svm_under_pre)
svm_over = criterion(Y_test,svm_over_pre)
svm_SMOTE = criterion(Y_test,svm_SMOTE_pre)
svm_raw= criterion(Y_test,svm_raw_pre)

rf_model_over<-randomForest(X_train_over, Y_train_over,ntree = 1000, mtry = 30, nodesize = 5,keep.forest=T)    
pred_y_over = predict(rf_model,newdata=X_test)
rf_over = criterion(Y_test,pred_y_over)

rf_model_under<-randomForest(X_train_under, Y_train_under,ntree = 1000, mtry = 30, nodesize = 5,keep.forest=T)    
pred_y_under = predict(rf_model,newdata=X_test)
rf_under = criterion(Y_test,pred_y_under)

rf_model_SMOTE<-randomForest(X_train_SMOTE, Y_train_SMOTE,ntree = 1000, mtry = 30, nodesize = 5,keep.forest=T)    
pred_y_SMOTE = predict(rf_model,newdata=X_test)
rf_SMOTE = criterion(Y_test,pred_y_SMOTE)

rf_model_raw<-randomForest(X_train, Y_train,ntree = 1000, mtry = 30, nodesize = 5,keep.forest=T)    
pred_y_raw = predict(rf_model,newdata=X_test)
rf_raw = criterion(Y_test,pred_y_raw)


