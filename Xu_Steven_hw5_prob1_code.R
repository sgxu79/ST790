#Data from Homework 4###############
library(mvtnorm)
library(MASS)
set.seed(2000)
mu_g = c(2,1)
mu_r = c(1,2)
generate_dots = function(n_samp){
  d = cbind(rmvnorm(n_samp,c(2,1)),rep(0,n_samp))
  d = rbind(d,cbind(rmvnorm(n_samp,c(1,2)),rep(1,n_samp)))
  colnames(d) = c("x1","x2","y")
  return(data.frame(d))
}
n_train = 100
dtrain = generate_dots(n_train)
plot(NULL,NULL,xlim=c(-3,5),ylim=c(-2,5),xlab="",ylab="")
points(subset(dtrain,y==0)[,-3],col="green")
points(subset(dtrain,y==1)[,-3],col="red")

set.seed(2014)
n_test = 500
dtest = generate_dots(n_test)
####################################

clas_err = function(ans,est){
  result = table(ans,est)
  b_fp = result[1,2]/sum(result[1,])
  b_fn = result[2,1]/sum(result[2,])
  b_mc = (result[1,2]+result[2,1])/sum(result)
  output = c(b_fp,b_fn,b_mc)
  names(output) = c("False Positive","False Negative","Classfication Error")
  return(output)
}


fit_lda = lda(y~x1+x2,data = dtrain)
pre_train_lda = predict(fit_lda,dtrain)$class
pre_test_lda = predict(fit_lda,dtest)$class
clas_err(dtrain$y,pre_train_lda)
clas_err(dtest$y,pre_test_lda)


fit_log = glm(y~x1+x2,family = binomial(link="logit"),data = dtrain)
pre_train_log = ifelse(fit_log$fitted.values>0.5,1,0)
pre_test_log = ifelse(predict(fit_log,dtest,type="response")>0.5,1,0)
clas_err(dtrain$y,pre_train_log)
clas_err(dtest$y,pre_test_log)



