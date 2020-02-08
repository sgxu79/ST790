library(e1071)
library(mvtnorm)

#Code for generating scenario 2
set.seed(2000)
m_g = c(1,0)
m_r = c(0,1)
mu = rmvnorm(10,m_g,diag(2))
nu = rmvnorm(10,m_r,diag(2))
train_g = do.call(rbind,lapply(seq_len(100),function(i){rmvnorm(1,mu[sample(1:10,1),],diag(2)/5)}))
train_r = do.call(rbind,lapply(seq_len(100),function(i){rmvnorm(1,nu[sample(1:10,1),],diag(2)/5)}))
dtrain = cbind(rbind(train_g,train_r),rep(c(0,1),each=100))
colnames(dtrain) = c("x1","x2","y")
dtrain = as.data.frame(dtrain)
plot(dtrain$x1,dtrain$x2,xlab="x1",ylab="x2")
points(train_g,col="green")
points(train_r,col="red")

set.seed(2014)
test_g = do.call(rbind,lapply(seq_len(500),function(i){rmvnorm(1,mu[sample(1:10,1),],diag(2)/5)}))
test_r = do.call(rbind,lapply(seq_len(500),function(i){rmvnorm(1,nu[sample(1:10,1),],diag(2)/5)}))
dtest = cbind(rbind(test_g,test_r),rep(c(0,1),each=500))
colnames(dtest) = c("x1","x2","y")
dtest = as.data.frame(dtest)


dtrain$y = factor(dtrain$y)
dtest$y = factor(dtest$y)

#We will use the built-in tune function for svm.
#Since different types of kernel have different parameters they will be tuned separately.
#Linear kernel

svm_lin_fit = tune(svm, y~x1+x2, data = dtrain, validation.x = dtest,
                   ranges = list(cost = 2^seq(-8,8,length.out = 50)),
                   tunecontrol = tune.control(sampling = "fix"),kernel = "linear")


svm_lin_fit

#Polynomial kernel
#Some parameter combinations results in max iteration warning, so they were discarded

svm_poly_fit = tune(svm, y~x1+x2, data = dtrain, validation.x = dtest,
                   ranges = list(degree = 1:6, gamma = seq(0.001,1,length.out = 10),coef0 = seq(0,3,0.2),cost = 2^seq(-6,3,length.out = 10)),
                   tunecontrol = tune.control(sampling = "fix"),kernel = "polynomial")

svm_poly_fit

#rbf kernel

svm_rbf_fit = tune(svm, y~x1+x2, data = dtrain, validation.x = dtest,
                    ranges = list(gamma = 2^seq(-15,4,length.out = 50),cost = 2^seq(-5,5,length.out = 50)),
                    tunecontrol = tune.control(sampling = "fix"),kernel = "radial")

svm_rbf_fit
