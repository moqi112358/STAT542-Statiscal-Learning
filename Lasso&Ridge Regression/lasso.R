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
      p = sum(X[,j] * (Y - X[,-j] %*% mybeta[-j]))
      #update rule / soft thresholding function
      mybeta[j] = sign(p) * ifelse(abs(p)>n*mylambda,abs(p)-n*mylambda,0) / z[j]
      #r = r + X[,j] * mybeta[j]
      #p = sum(X[,j] * r)
      #mybeta[j] = sign(p) * ifelse(abs(p)>n*mylambda,abs(p)-n*mylambda,0) / z[j]
      #r = r - X[,j] * mybeta[j]
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
#scale
x_center = colMeans(X)
x_scale = apply(X, 2, sd)
X2 = scale(X)
bhat = rep(0, ncol(X2)) # initialize it
ymean = mean(y)
y2 = y - ymean
lambda = exp(seq(log(max(abs(cov(X2, y2)))), log(0.001), length.out = 100))

# now initiate a matrix that records the fitted beta for each lambda value 
beta_all = matrix(NA, ncol(X), length(lambda))

# this vecter stores the intercept of each lambda value
beta0_all = rep(NA, length(lambda))

# this part gets pretty tricky: you will initial a zero vector for bhat, 
# then throw that into the fit function using the largest lambda value. 
# that will return the fitted beta, then use this beta on the next (smaller) lambda value
# iterate until all lambda values are used

bhat = rep(0, ncol(X)) # initialize it

for (i in 1:length(lambda)){ # loop from the largest lambda value
  lasso_beta = LassoFit(X, y, bhat, lambda[i])
  bhat = lasso_beta$beta
  beta_all[, i] = bhat
  beta0_all[i] = lasso_beta$beta_0
}

# now you have the coefficient matrix 
# each column correspond to one lambda value 
rbind("intercept" = beta0_all, beta_all)

# you should include a similar plot like this in your report
# feel free to make it look better
matplot(colSums(abs(beta_all)), t(beta_all), type="l")
# this plot should be identical (close) to your previous plot
plot(glmnet(X, y))

# set your lambda to their lambda value and rerun your algorithm 
lambda = glmnet(X, y)$lambda

beta_all = matrix(NA, ncol(X), length(lambda))

beta0_all = rep(NA, length(lambda))

bhat = rep(0, ncol(X)) # initialize it

for (i in 1:length(lambda)){ # loop from the largest lambda value
  lasso_beta = LassoFit(X, y, bhat, lambda[i])
  bhat = lasso_beta$beta
  beta_all[, i] = bhat
  beta0_all[i] = lasso_beta$beta_0
}

# then this distance should be pretty small 
# my code gives distance no more than 0.01
max(abs(beta_all - glmnet(X, y)$beta))
max(abs(beta0_all - glmnet(X, y)$a0))
