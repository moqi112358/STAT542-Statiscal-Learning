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

# check OLS
lm(y ~ X)

# now start the Lasso 
# First we scale and center X, and record them.
# Also center y and record it. dont scale it. 
# now since both y and X are centered at 0, we don't need to worry about the intercept anymore. 
# this is because for any beta, X %*% beta will be centered at 0, so no intercept is needed. 
# However, we still need to recover the real intercept term after we are done estimating the beta. 
# The real intercept term can be recovered by using the x_center, x_scale, y2, and the beta parameter you estimated.
# There are other simpler ways to do it too, if you think carefully. 

x_center = colMeans(X)
x_scale = apply(X, 2, sd)
X2 = scale(X)

bhat = rep(0, ncol(X2)) # initialize it
ymean = mean(y)
y2 = y - ymean

# now start to write functions 
# prepare the soft thresholding function (should be just one line, or a couple of)

soft_th <- function(b, pen)
{
	???
}

# initiate lambda. This is one way to do it, the logic is that I set the first lambda as the largetst gradient. 
# if you use this formula, you will need to calculate this for the real data too.

lambda = exp(seq(log(max(abs(cov(X2, y2)))), log(0.001), length.out = 100))

# you should write the following function which can be called this way 
LassoFit(X2, y2, mybeta = rep(0, ncol(X2)), mylambda = lambda[10])

LassoFit <- function(myX, myY, mybeta, mylambda, tol = 1e-10, maxitr = 500)
{
	# initia a matrix to record the objective function value
	f = rep(0, maxitr)
	
	for (k in 1:maxitr)
	{
		# compute residual
		r = ???
		
		# I need to record the residual sum of squares
		f[k] = mean(r*r)
		
		for (j in 1:ncol(myX))
		{
			# add the effect of jth variable back to r 
			# so that the residual is now the residual after fitting all other variables
			???
			
			# apply the soft thresholding function to the ols estimate of the jth variable 
			???
			
			# remove the new effect of jth varaible out of r
			???
		}
		
		if (k > 10)
		{
			# this is just my adhoc way of stoping rule, you dont have to use it
			if (sum(abs(f[(k-9):k] - mean(f[(k-9):k]))) < tol) break;
		}
	}
	return (mybeta)
}

# you should test your function on a large lambda (penalty) level. 
# this should produce a very spase model. 
# keep in mind that these are not the beta in the original scale of X

LassoFit(X2, y2, mybeta = rep(0, ncol(X2)), mylambda = lambda[10], tol = 1e-7, maxitr = 500)

# now initiate a matrix that records the fitted beta for each lambda value 

beta_all = matrix(NA, ncol(X), length(lambda))

# this vecter stores the intercept of each lambda value
beta0_all = rep(NA, length(lambda))

# this part gets pretty tricky: you will initial a zero vector for bhat, 
# then throw that into the fit function using the largest lambda value. 
# that will return the fitted beta, then use this beta on the next (smaller) lambda value
# iterate until all lambda values are used

bhat = rep(0, ncol(X2)) # initialize it

for (i in 1:length(lambda)) # loop from the largest lambda value
{
	# if your function is correct, this will run pretty fast
	
	bhat = LassoFit(X2, y2, bhat, lambda[i])
	
	# this is a tricky part, since your data is scaled, you need to figure out how to scale that back 
	# save the correctly scaled beta into the beta matrix 
	
	beta_all[, i] = ???
	
	# here, you need to figure out a way to recalculte the intercept term in the original, uncentered and unscaled X

	beta0_all[i] = ???
}


# now you have the coefficient matrix 
# each column correspond to one lambda value 
rbind("intercept" = beta0_all, beta_all)


# you should include a similar plot like this in your report
# feel free to make it look better
matplot(colSums(abs(beta_all)), t(beta_all), type="l")






# The following part provides a way to check your code. 
# You do not need to include this part in your report. 

# However, keep in mind that my original code is based on formula (3)
# if you use other objective functions, it will be different, and the results will not match
 
# load the glmnet package and get their lambda 
library(glmnet)

# this plot should be identical (close) to your previous plot
plot(glmnet(X, y))

# set your lambda to their lambda value and rerun your algorithm 
lambda = glmnet(X, y)$lambda

# then this distance should be pretty small 
# my code gives distance no more than 0.01
max(abs(beta_all - glmnet(X, y)$beta))
max(abs(beta0_all - glmnet(X, y)$a0))












