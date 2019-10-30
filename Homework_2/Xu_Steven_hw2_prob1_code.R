if(!require(mvtnorm)){install.packages("mvtnorm")}
library(mvtnorm)
n = 100
d = 3
x = sort(runif(n,-1,1))
X_mat = cbind(1,x,x^2,x^3)
beta = c(1,-1,5,2)
y = X_mat%*%beta+rnorm(100)
plot(x,y)
fit = lm(y~X_mat-1)
beta_hat = fit$coefficients
sigma_hat = summary(fit)$sigma
b_cov = sigma_hat^2*summary(fit)$cov.unscaled
alpha = 0.05
band.1 = matrix(0,nrow=n,ncol=2)
for(i in 1:n){
  band.1[i,] = as.numeric(X_mat[i,]%*%beta_hat)+qnorm(alpha/2)*c(1,-1)*as.numeric(sqrt(t(X_mat[i,])%*%b_cov%*%X_mat[i,]))
}
y_hat = fit$fitted.values
lines(x,y_hat,lwd=2)
lines(x,band.1[,1],col=2,lty=2,lwd=2)
lines(x,band.1[,2],col=2,lty=2,lwd=2)
thresh = sigma_hat^2*qchisq(1-alpha,d+1)
b_samp = rmvnorm(10000,mean=beta_hat,sigma=b_cov)
b_samp_95 = {}
for(i in 1:10000){
  val = t(beta_hat-b_samp[i,])%*%(t(X_mat)%*%X_mat)%*%(beta_hat-b_samp[i,])
  if(val<=thresh){
    b_samp_95 = cbind(b_samp_95,b_samp[i,])
  }
}

# band.2 = X_mat%*%t(b_samp)
# band.2 = apply(band.2,1,quantile,c(0.025,0.975))
# lines(x,band.2[1,],col=3,lty=3)
# lines(x,band.2[2,],col=3,lty=3)
# 

band.2 = X_mat%*%b_samp_95
band.2 = cbind(apply(band.2,1,min),apply(band.2,1,max))
lines(x,band.2[,1],col=3,lty=3,lwd=2)
lines(x,band.2[,2],col=3,lty=3,lwd=2)
legend("bottomright", legend=c("Fitted","Method (i)", "Method (ii)"),
       col=c("black","red", "green"), lty=1:3,lwd=2)
