#############################Problem 1##########################

#############################Problem 1a#########################
library(leaps)

#read data set
data = read.csv('bitcoin_dataset.csv')

#get the index of train and test data
index1 = which(data[,1] == '2017-01-01 00:00:00')
index2 = which(data[,1] == '2017-09-12 00:00:00')

#Here we treat it as a continuous variable, say starting with 
#value 0 at 2014/1/1, then increase by 1 for each two days.
data$Date = 1:dim(data)[1]

#split the data set into train and test data
#We remove Date information and treat the 2rd column 'btc_market_price'
#as the outcome variable
train_data = data[1:index1-1,]
test_data = data[index1:index2,]
train_data_x = train_data[,-2]
train_data_y = train_data[,2]
test_data_x = test_data[,-2]
test_data_y = test_data[,2]

#Ignore the variable btc_trade_volume because it contains missing values
train_data_x = subset(train_data_x,select = -btc_trade_volume)
test_data_x = subset(test_data_x,select = -btc_trade_volume)

# performs an exhaustive search over models, and gives back the best model 
# (with low RSS) of each size.

RSSleaps=regsubsets(as.matrix(train_data_x),train_data_y,nvmax = length(train_data_x))
best_subset= summary(RSSleaps, matrix=T)
t(best_subset$which)


#############################Problem 1b#########################
lmfit=lm(train_data_y~as.matrix(train_data_x))
msize=apply(best_subset$which,1,sum)
n=dim(train_data_x)[1]
p=dim(train_data_x)[2]
#calculate the Cp, AIC, BIC of the best model of each size
Cp = best_subset$rss/(summary(lmfit)$sigma^2) + 2*msize - n
AIC = n*log(best_subset$rss/n) + 2*msize
BIC = n*log(best_subset$rss/n) + msize*log(n)

which.min(Cp)
which.min(AIC)
which.min(BIC)
cbind(Cp,AIC,BIC)[c(10,11),]
names(train_data_x)[best_subset$which[10,-1]]
names(train_data_x)[best_subset$which[11,-1]]

# Rescale Cp, AIC, BIC to (0,1).
inrange <- function(x) { (x - min(x)) / (max(x) - min(x)) }
Cp = best_subset$cp; Cp = inrange(Cp);
BIC = best_subset$bic; BIC = inrange(BIC);
AIC = n*log(best_subset$rss/n) + 2*msize; AIC = inrange(AIC);

#plot the Model Selection Criteria(Cp, AIC, BIC) of each size
plot(range(msize), c(0, 1.1), type="n", xlab="Model Size (with Intercept)", ylab="Model Selection Criteria")
points(msize, Cp, col="red", type="b")
points(msize, AIC, col="blue", type="b")
points(msize, BIC, col="black", type="b")
legend("topright", lty=rep(1,3), col=c("red", "blue", "black"), legend=c("Cp", "AIC", "BIC"))

#zoom in 
id=6:15;
plot(range(msize[id]), c(0, 0.02), type="n", xlab="Model Size (with Intercept)", ylab="Model Selection Criteria")
points(msize[id], Cp[id], col="red", type="b")
points(msize[id], AIC[id], col="blue", type="b")
points(msize[id], BIC[id], col="black", type="b")
legend("topright", lty=rep(1,3), col=c("red", "blue", "black"), legend=c("Cp", "AIC", "BIC"))

#applied the fitted models to the testing dataset 
model_10 = lm(btc_market_price~btc_total_bitcoins+btc_market_cap+btc_blocks_size+btc_avg_block_size+
                btc_n_orphaned_blocks+btc_hash_rate+btc_difficulty+btc_miners_revenue+
                btc_cost_per_transaction+btc_n_transactions_total,data=train_data)

predict_10_y = predict(model_10,newdata = test_data)

model_11 = lm(btc_market_price~Date+btc_total_bitcoins+btc_market_cap+btc_blocks_size+btc_avg_block_size+
                btc_n_orphaned_blocks+btc_hash_rate+btc_difficulty+btc_miners_revenue+btc_cost_per_transaction+
                btc_n_transactions_total,data=train_data)

predict_11_y = predict(model_11,newdata = test_data)

# Calculate the prediction error
mse_10 = mean((predict_10_y - test_data_y)^2)
mse_11 = mean((predict_11_y - test_data_y)^2)
# The MSE of the best model with size = 10 (BIC)
mse_10
# The MSE of the best model with size = 11 (Cp, AIC)
mse_11 

#############################Problem 1c#########################

# redo a-b
# Performs an exhaustive search over models, and gives back the best model 
# (with low RSS) of each size.
RSSleaps=regsubsets(as.matrix(train_data_x),log(train_data_y+1),
                    nvmax = length(train_data_x))
best_subset= summary(RSSleaps, matrix=T)

# Calculate the Cp, AIC, BIC of the best model of each size
lmfit=lm(log(train_data_y+1)~as.matrix(train_data_x))
msize=apply(best_subset$which,1,sum)
n=dim(train_data_x)[1]
p=dim(train_data_x)[2]

Cp = best_subset$rss/(summary(lmfit)$sigma^2) + 2*msize - n
AIC = n*log(best_subset$rss/n) + 2*msize
BIC = n*log(best_subset$rss/n) + msize*log(n)

#the best model
which.min(Cp)
which.min(AIC)
which.min(BIC)

cbind(Cp,AIC,BIC)[c(13,14),]
# Selected features in the best model
names(train_data_x)[best_subset$which[13,-1]]
names(train_data_x)[best_subset$which[14,-1]]

# Apply the fitted models to the testing dataset 
model_log_13 = lm(log(btc_market_price+1)~.,data=train_data[,c('btc_market_price',
                                                               names(which(best_subset$which[13,-1] == TRUE)))])

predict_log_13_y = predict(model_log_13,newdata = test_data)

model_log_14 = lm(log(btc_market_price+1)~.,data=train_data[,c('btc_market_price',
                                                               names(which(best_subset$which[14,-1] == TRUE)))])

predict_log_14_y = predict(model_log_14,newdata = test_data)

# Calculate the prediction error
e = 2.718281828459
mse_log_13 = mean(((e^(predict_log_13_y)-1) - test_data_y)^2)
mse_log_14 = mean(((e^(predict_log_14_y)-1) - test_data_y)^2)
# The MSE of the best model with size = 13 (BIC)
mse_log_13 
# The MSE of the best model with size = 14 (Cp, AIC)
mse_log_14

#############################Problem 2#########################

#############################Problem 2 Part I#########################

# Function LassoFit
# Usage:
# LassoFit(X, y, mybeta = rep(0, ncol(X)), lambda, tol = 1e-10, maxitr = 500)

# Arguments
# myX: input matrix, each row is an observation vector (don't need to be 
#      scaled and centered)
# myY: response variable (don't need to be centered)
# mybeta: the initialization of beta
# mylambda: tuning parameter (penalty level), which controls the amount of  
#      shrinkage. Usually, mylambda >= 0.
# tol: Convergence threshold for coordinate descent. Each inner coordinate- 
#      descent loop continues until the change in the objective function value  
#      after any update is less than tol (or run maxitr iterations). Defaults 
#      value is 1E-10.
# maxitr: The maximum number of iteration in coordinate-descent loop. Defaults 
#      value is 500.

# Value(Output)
# beta_0: The intercept term in our fitted model of the original scale of X.
# beta: The coefficient in our fitted model of the original scale of X.

LassoFit <- function(myX, myY, mybeta, mylambda, tol = 1e-10, maxitr = 500){
  # First we scale and center X, and record them.
  # Also center y and record it. dont scale it. 
  # now since both y and X are centered at 0, we don't need to worry about the intercept anymore. 
  # this is because for any beta, X %*% beta will be centered at 0, so no intercept is needed. 
  # However, we still need to recover the real intercept term after we are done estimating the beta. 
  # The real intercept term can be recovered by using the x_center, x_scale, y, 
  # and the beta parameter you estimated.
  x_center = colMeans(myX)
  x_scale = apply(myX, 2, sd)
  X = scale(myX)
  y_mean = mean(myY)
  Y = myY - y_mean
  # Calculate the number of instances
  n = nrow(X)
  # Calculate z = \sum (x_ij)^2
  z = apply(X,2,function(x) sum(x^2))
  # Initia a matrix to record the objective function value
  f = rep(0, maxitr)
  for (k in 1:maxitr){
    # Compute the residual using current beta
    r = Y - X %*% mybeta
    # Record the objective function value
    f[k] = sum(r*r) / (2*n) + mylambda * sum(abs(mybeta))
    # Calculate the stopping rule
    if (k > 10){
      if (abs(f[k] - f[k-1]) < tol) break;
    }
    # Use coordinate descent to update beta
    for (j in 1:ncol(X)){
      #p = sum(X[,j] * (Y - X[,-j] %*% mybeta[-j]))
      #update rule / soft thresholding function
      #mybeta[j] = sign(p) * ifelse(abs(p)>n*mylambda,abs(p)-n*mylambda,0) / z[j]
      r = r + X[,j] * mybeta[j]
      p = sum(X[,j] * r)
      mybeta[j] = sign(p) * ifelse(abs(p)>n*mylambda,abs(p)-n*mylambda,0) / z[j]
      r = r - X[,j] * mybeta[j]
    }
  }
  # Scale the beta back 
  mybeta = mybeta / x_scale
  # Recalculte the intercept term in the original, uncentered and unscaled X
  beta_0 = mean(myY - myX %*% mybeta)
  return(list("beta" = mybeta,"beta_0" = beta_0))
}
library(MASS)
library(glmnet)
set.seed(1)
N = 400
P = 20
Beta = c(1:5/5, rep(0, P-5))
Beta0 = 0.5
# genrate X
V = matrix(0.5, P, P)
diag(V) = 1
X = as.matrix(mvrnorm(N, mu = 3*runif(P)-1, Sigma = V))
# create artifical scale of X
X = sweep(X, 2, 1:10/5, "*")
# genrate Y
y = Beta0 + X %*% Beta + rnorm(N)

#set the lambda sequence
lambda = exp(seq(log(max(abs(cov(scale(X), (y-mean(y)))))), log(0.001), 
                 length.out = 100))
# now initiate a matrix that records the fitted beta for each lambda value 
beta_all = matrix(NA, ncol(X), length(lambda))
# this vecter stores the intercept of each lambda value
beta0_all = rep(NA, length(lambda))
# Here we will initial a zero vector for bhat, then throw that into the 
# fit function using the largest lambda value. that will return the fitted 
# beta, then use this beta on the next (smaller) lambda value iterate until 
# all lambda values are used

# Initialize beta
bhat = rep(0, ncol(X)) 
# loop from the largest lambda value
for (i in 1:length(lambda)){ 
  lasso_beta = LassoFit(X, y, bhat, lambda[i])
  bhat = lasso_beta$beta
  beta_all[, i] = bhat
  beta0_all[i] = lasso_beta$beta_0
}
# Here we make a plot of the L1 Norm versus Coefficients
matplot(colSums(abs(beta_all)), t(beta_all), type="l",xlab = "L1 Norm", 
        ylab = "Coefficients")
# We can also make a plot from the result of glmnet
plot(glmnet(X, y, lambda = lambda, alpha = 1))

# Set our lambda to their lambda value and rerun your algorithm 
lambda = glmnet(X, y)$lambda
beta_all = matrix(NA, ncol(X), length(lambda))
beta0_all = rep(NA, length(lambda))
# Initialize beta
bhat = rep(0, ncol(X)) 
# loop from the largest lambda value
for (i in 1:length(lambda)){ 
  lasso_beta = LassoFit(X, y, bhat, lambda[i])
  bhat = lasso_beta$beta
  beta_all[, i] = lasso_beta$beta
  beta0_all[i] = lasso_beta$beta_0
}

# then this distance should be pretty small 
# my code gives distance no more than 0.01
glmnet_result = glmnet(X, y, alpha = 1)
max(abs(beta_all - glmnet_result$beta))
max(abs(beta0_all - glmnet_result$a0))



#############################Problem 2 Part II#########################

#Set our lambda to the lambda value from glmnet
result = glmnet(as.matrix(train_data_x),train_data_y,alpha = 1)
lambda = result$lambda
# Initiate a matrix that records the fitted beta for each lambda value 
beta_all = matrix(NA, ncol(train_data_x), length(lambda))
# This vecter stores the intercept of each lambda value
beta0_all = rep(NA, length(lambda))
# This vecter stores the test error of each lambda value
MSE = rep(NA, length(lambda))
# Initialize beta
bhat = rep(0, ncol(train_data_x)) 
# Loop from the largest lambda value
for (i in 1:length(lambda)){ 
  # Train the Lasso model
  lasso_beta = LassoFit(as.matrix(train_data_x), train_data_y, bhat, lambda[i])
  # Store the coefficients and intercepts
  beta_all[, i] = lasso_beta$beta
  beta0_all[i] = lasso_beta$beta_0
  # Apply the fitted model on the test dataset 
  test_y_hat = as.matrix(test_data_x) %*% lasso_beta$beta + lasso_beta$beta_0
  #Calculate the test error
  MSE[i] = mean((test_y_hat-test_data_y)^2)
}
cbind(lambda,MSE)
# Make plots of lambda versus MSE
plot(MSE~lambda)
# Zoom in
sort_result = cbind(lambda,MSE)[order(cbind(lambda,MSE)[,2],decreasing=F),]
plot(MSE~lambda,data = sort_result[1:12,],pch=19)
lines(MSE~lambda)

# The test error and lambda of the best model
cbind(lambda,MSE)[which.min(MSE),]
# Report the coefficient and intercept of the best model.
best_model = data.frame(c(beta_all[,which.min(MSE)],beta0_all[which.min(MSE)]),
                        row.names = c(names(train_data_x),"Intercept"))
colnames(best_model) = c("Coefficient")
best_model 

matplot(colSums(abs(beta_all)), t(beta_all), type="l",xlab = "L1 Norm", 
        ylab = "Coefficients",lty = 1)

#Finally, we make a plot of fitted Y values versus the true value of y. We can find that lasso perform pretty well on the data set.

fitted_y_best_model = as.matrix(test_data_x) %*% best_model[1:22,1] + best_model[23,1]
plot(test_data_y,fitted_y_best_model,xlab = "True value of Y", 
     ylab = "Fitted value of Y",pch = 19,cex=0.5)
abline(0,1,col="red")