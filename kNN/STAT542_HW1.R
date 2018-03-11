#STAT542 HW1 
#Name:Huamin Zhang(huaminz2)

###############Problem 1#######################
library(MASS)
set.seed(1)
P = 4
N = 200
rho = 0.5
V <- rho^abs(outer(1:P, 1:P, "-"))
X = as.matrix(mvrnorm(N, mu=rep(0,P), Sigma=V))
beta = as.matrix(c(1, 1, 0.5, 0.5))
Y = X %*% beta + rnorm(N)

###############Problem 1.a#######################
one = as.matrix(rep(1,N))
x = X - 1/N * one %*% t(one) %*% X 
var_cov_a = t(x) %*% x * (1/N)

#var_cov_a = cov(X)*(N-1)/N

eigenvector <- eigen(var_cov_a) 
var_cov_b = eigenvector$vectors %*% diag(eigenvector$values^(-1/2)) %*% t(eigenvector$vectors) 
var_cov_b

###############Problem 1.b#######################
mydist <- function(x1, x2){
  return(sqrt(sum((x1-x2)^2)))
}

x = c(0.5,0.5,0.5,0.5)
Euclidean_distance = rep(0,N)
for(i in 1:N){
  Euclidean_distance[i] = mydist(x,X[i,])
}
closet_id = order(Euclidean_distance)[1:5]
closet_id
Euclidean_distance[closet_id]
y = mean(Y[closet_id])
y

###############Problem 1.c#######################
mydist2 <- function(x1, x2, s){
  x = x1 - x2
  d2 = t(x) %*% solve(var_cov_a) %*% x
  return(sqrt(d2))
}
Mahalanobis_distance = rep(0,N)
for(i in 1:N){
  Mahalanobis_distance[i] = mydist2(x,X[i,],var_cov_a)
}
closet_id2 = order(Mahalanobis_distance)[1:5]
closet_id2
Mahalanobis_distance[closet_id2]
y2 = mean(Y[closet_id2])
y2

###############Problem 1.d#######################
set.seed(10086)
a = rep(0,50)
b = rep(0,50)

Euclidean <- function(X1,Y1,X,Y,z){
  y1 = rep(0,200)
  for(k in 1:200){
    Euclidean_distance = rep(0,2000)
    for(i in 1:2000){
      Euclidean_distance[i] = mydist(X1[k,],X[i,])
    }
    closet_id = order(Euclidean_distance)[1:z]
    y = mean(Y[closet_id])
    y1[k] = y
  }
  dis = sum(Y1-y1)^2 / 200
  return(dis)
}

Mahalanobis <- function(X1,Y1,X,Y,s,z){
  y1 = rep(0,200)
  for(k in 1:200){
    Mahalanobis_distance = rep(0,2000)
    for(i in 1:2000){
      Mahalanobis_distance[i] = mydist2(X1[k,],X[i,],s)
    }
    closet_id = order(Mahalanobis_distance)[1:z]
    y = mean(Y[closet_id])
    y1[k] = y
  }
  dis = sum(Y1-y1)^2 / 200
  return(dis)
}
for(p in 1:50){
  P = 4
  N = 2200
  rho = 0.5
  V <- rho^abs(outer(1:P, 1:P, "-"))
  X = as.matrix(mvrnorm(N, mu=rep(0,P), Sigma=V))
  beta = as.matrix(c(1, 1, 0.5, 0.5))
  Y = X %*% beta + rnorm(N)
  X1 = X[1:200,]
  Y1 = Y[1:200]
  X = X[201:2200,]
  Y = Y[201:2200]
  z = 5
  
  one = as.matrix(rep(1,2000))
  x = X - 1/2000 * one %*% t(one) %*% X 
  var_cov_a = t(x) %*% x * (1/2000)
  
  a[p] = Euclidean(X1,Y1,X,Y,z)
  b[p] = Mahalanobis(X1,Y1,X,Y,var_cov_a,z)
}

mean(a)
mean(b)
###############Problem 2#######################

###############Problem 2.b#######################
library(kknn)
set.seed(2)
P = 4
N = 200
I = diag(nrow = 4)
X = as.matrix(mvrnorm(N, mu=rep(0,P), Sigma=I))
beta = as.matrix(c(1, 1, 0.5, 0.5))
Y = X %*% beta + rnorm(N)

k = 5
knn.fit = kknn(Y ~ X, train = data.frame(x = X, y = Y), test = data.frame(x = X, y = Y),
               k = k, kernel = "rectangular")
test.pred = knn.fit$fitted.values

Y.pred = NULL
Y.train = NULL
for(i in 1:20){
  Y = X %*% beta + rnorm(N)
  Y.train = cbind(Y.train, Y)
  knn.fit = kknn(Y ~ X, train = data.frame(x = X, y = Y), test = data.frame(x = X, y = Y),
                 k = k, kernel = "rectangular")
  Y.pred = cbind(Y.pred, knn.fit$fitted.values)
}

sum_cov = 0
for(j in 1:N){
  sum_cov = sum_cov + cov(Y.pred[j,],Y.train[j,])
}
df = sum_cov / 1
df

###############Problem 3#######################

set.seed(0)
library(ElemStatLearn)
library(class)
head(SAheart)
x = SAheart[,c('age','tobacco')]
y = SAheart[,'chd']

nfold = 10
infold = sample(rep(1:nfold, length.out=dim(x)[1]))

K = floor(dim(x)[1] *9 / 10) # maximum number of k that I am considering
train_errorMatrix = matrix(NA, K, nfold) # save the prediction error of each fold
test_errorMatrix = matrix(NA, K, nfold) # save the prediction error of each fold

for (l in 1:nfold){
  for (k in 1:K){
    knn.train <- knn(train = x[infold != l, ], test = x[infold != l, ], cl = y[infold != l], k=k)
    knn.test <- knn(train = x[infold != l, ], test = x[infold == l, ], cl = y[infold != l], k=k)
    train_errorMatrix[k,l] = mean(knn.train != y[infold != l])
    test_errorMatrix[k, l] = mean(knn.test != y[infold == l])
  }
}
avg_train_error = apply(train_errorMatrix,1,mean)
avg_test_error = apply(test_errorMatrix,1,mean)

head(avg_train_error)
best_k = order(avg_test_error)[1]
best_k
avg_train_error[best_k]
avg_test_error[best_k]


#use all data refit the model with best k
knn.best_k <- knn(train = x, test = x, cl = y, k=best_k)
knn_train_error.best_k = mean(knn.best_k != y)
knn_train_error.best_k

plot(avg_train_error)
plot(avg_test_error,pch = 19,cex=0.5,col='green',xlab = 'The value of k (number of neighbor)',
     ylab = 'Average error', main = ' The averaged training/cross-validation error curve for difference choices of k',ylim=c(0.2,0.42))
abline(v = order(avg_test_error)[1])
points(avg_train_error,pch = 19,cex=0.5,col='red')
legend( "topright", c("The averaged training error ","The averaged cross-validation error"), pch = 19, col = c('red','green'), cex = 1)
