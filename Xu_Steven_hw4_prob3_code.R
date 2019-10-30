library(mvtnorm)
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
save(dtrain,file="~/hw4_train.rda")
save(dtest,file="~/hw4_test.rda")

