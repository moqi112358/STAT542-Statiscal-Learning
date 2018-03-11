rm(list=ls())
library(glmnet)
library(randomForest)
library(caret)
# read data
data = read.csv('tmdb_5000_movies_step2_with_keyword.csv')
data = data[-which(data$revenue_budget_ratio > 100),]
#head(data)
#colnames(data)

##########revenue as Y########################################
Y <- data$revenue
#Y <- data$revenue_budget_ratio
X <- data[, -c(2, 3, 5, 1887)] #with keyword

# odd id as train, even id as test
X_train <- X[data$id %% 2 == 1, ]
X_test <- X[data$id %% 2 == 0, ]
Y_train <- Y[data$id %% 2 == 1]
Y_test <- Y[data$id %% 2 == 0]

# select the train x whose correlation with y is not less than 0.1
X_test <- X_test[,colSums(X_train) > 1]
X_train <- X_train[,colSums(X_train) > 1]
r <- abs(cor(Y_train, X_train))
X_train <- X_train[, r >= 0.1]
X_test <- X_test[, r >= 0.1]

# lasso
X_train <- as.matrix(X_train)
X_test <- as.matrix(X_test)
lasso1.cv <- cv.glmnet(X_train, Y_train, alpha = 1, nfolds = 10, type.measure = 'mae')
lasso1 <- glmnet(X_train, Y_train, alpha = 1, lambda = lasso1.cv$lambda.min)
Y_pre_lasso1 <- predict(lasso1, X_test)
error_lasso1 <- mean(abs(Y_pre_lasso1 - Y_test))

##########revenue_budget_ratio as Y########################################
#Y <- data$revenue
Y <- data$revenue_budget_ratio
X <- data[, -c(2, 3, 5, 1887)] #with keyword

# odd id as train, even id as test
X_train <- X[data$id %% 2 == 1, ]
X_test <- X[data$id %% 2 == 0, ]
Y_train <- Y[data$id %% 2 == 1]
Y_test <- Y[data$id %% 2 == 0]

# select the train x whose correlation with y is not less than 0.1
X_test <- X_test[,colSums(X_train) > 1]
X_train <- X_train[,colSums(X_train) > 1]
r <- abs(cor(Y_train, X_train))
X_train <- X_train[, r >= 0.1]
X_test <- X_test[, r >= 0.1]

# lasso
X_train <- as.matrix(X_train)
X_test <- as.matrix(X_test)
lasso2.cv <- cv.glmnet(X_train, Y_train, alpha = 1, nfolds = 10, type.measure = 'mae')
lasso2 <- glmnet(X_train, Y_train, alpha = 1, lambda = lasso1.cv$lambda.min)
Y_pre_lasso2 <- predict(lasso2, X_test)
error_lasso2 <- mean(abs(Y_pre_lasso2 * data$budget[data$id %% 2 == 0] - data$revenue_budget_ratio[data$id %% 2 == 0]))


#Random Forest
##########revenue as Y########################################
#choose the best parameter
Y <- data$revenue
#Y <- data$revenue_budget_ratio
X <- data[, -c(2, 3, 5, 1887)] #with keyword

# odd id as train, even id as test
X_train <- X[data$id %% 2 == 1, ]
X_test <- X[data$id %% 2 == 0, ]
Y_train <- Y[data$id %% 2 == 1]
Y_test <- Y[data$id %% 2 == 0]

# select the train x whose correlation with y is not less than 0.1
X_test <- X_test[,colSums(X_train) > 1]
X_train <- X_train[,colSums(X_train) > 1]
r <- abs(cor(Y_train, X_train))
X_train <- X_train[, r >= 0.1]
X_test <- X_test[, r >= 0.1]
mtry = c(10,20,30,50)
ntree = c(100,200,500,1000)
nodesize = c(5,10,25,50)
acc1 = matrix(0,64, 4)
p = 0
for(i in 1:length(mtry)){
  for(j in 1:length(ntree)){
    for(k in 1:length(nodesize)){
      p = p + 1
      cv_partition <- createDataPartition(Y_train, p=.9, list=FALSE)
      train_1_x = as.matrix(X_train[cv_partition,])
      train_validation_x = as.matrix(X_train[-cv_partition,])
      train_1_y = Y_train[cv_partition]
      train_validation_y = Y_train[-cv_partition]
      rf_model<-randomForest(train_1_x, train_1_y,ntree = ntree[j], mtry = mtry[i], nodesize = nodesize[k],keep.forest=T)
      pred_y = predict(rf_model,newdata=train_validation_x)
      acc1[p,4] = mean(abs(pred_y - train_validation_y))
      acc1[p,1] = ntree[j]; acc1[p,2] = mtry[i]; acc1[p,3] = nodesize[k]
    }
  }
}

set.seed(0)
Y <- data$revenue
#Y <- data$revenue_budget_ratio
X <- data[, -c(2, 3, 5, 1887)] #with keyword

# odd id as train, even id as test
X_train <- X[data$id %% 2 == 1, ]
X_test <- X[data$id %% 2 == 0, ]
Y_train <- Y[data$id %% 2 == 1]
Y_test <- Y[data$id %% 2 == 0]

# select the train x whose correlation with y is not less than 0.1
X_test <- X_test[,colSums(X_train) > 1]
X_train <- X_train[,colSums(X_train) > 1]
r <- abs(cor(Y_train, X_train))
X_train <- X_train[, r >= 0.1]
X_test <- X_test[, r >= 0.1]
rf1 = randomForest(X_train, Y_train, ntree = 500, mtry = 30, nodesize = 5) 
Y_pre_rf1 <- predict(rf1, X_test)
error_rf1 <- mean(abs(Y_pre_rf1 - Y_test))

#Random Forest
##########revenue_budget_ratio as Y########################################
#Y <- data$revenue
Y <- data$revenue_budget_ratio
X <- data[, -c(2, 3, 5, 1887)] #with keyword

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
mtry = c(10,20,30)
ntree = c(100,200,500,1000)
nodesize = c(5,10,25,50)
acc2 = matrix(0, 48, 4)
p = 0
for(i in 1:length(mtry)){
  for(j in 1:length(ntree)){
    for(k in 1:length(nodesize)){
      p = p + 1
      cv_partition <- createDataPartition(Y_train, p=.9, list=FALSE)
      train_1_x = as.matrix(X_train[cv_partition,])
      train_validation_x = as.matrix(X_train[-cv_partition,])
      train_1_y = Y_train[cv_partition]
      train_validation_y = Y_train[-cv_partition]
      rf_model<-randomForest(train_1_x, train_1_y,ntree = ntree[j], mtry = mtry[i], nodesize = nodesize[k],keep.forest=T)
      pred_y = predict(rf_model,newdata=train_validation_x)
      acc2[p,4] = mean(abs(pred_y - train_validation_y))
      acc2[p,1] = ntree[j]; acc2[p,2] = mtry[i]; acc2[p,3] = nodesize[k]
    }
  }
}

set.seed(0)
#Y <- data$revenue
Y <- data$revenue_budget_ratio
X <- data[, -c(2, 3, 5, 1887)] #with keyword

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
rf2 = randomForest(X_train, Y_train, ntree = 500, mtry = 30, nodesize = 5) 
Y_pre_rf2 <- predict(rf2, X_test)
error_rf2 <- mean(abs(Y_pre_rf2 - Y_test))

colnames(acc1) = c('ntree','mty','nodesize','acc')
colnames(acc2) = c('ntree','mty','nodesize','acc')

save(Y_pre_lasso1, Y_pre_lasso2, error_lasso1, error_lasso2, acc1, acc2 ,
     Y_pre_rf1, Y_pre_rf2, error_rf1, error_rf2, file = 'regression_tune.Rdata')

