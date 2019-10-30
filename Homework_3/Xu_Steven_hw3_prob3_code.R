library(lars)
prostate = read.table('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/prostate.data')
p = 8
pro_train = subset(prostate,train==TRUE)[,-10]
n_train = nrow(pro_train)
pro_test = subset(prostate,train==FALSE)[,-10]
n_test = nrow(pro_test)
y_train = pro_train[,9]
y_test = pro_test[,9]

#Standardize both training and testing data by training mean, norm
X_train = as.matrix(pro_train[,-9])
train_m = colMeans(X_train)
train_norm = sqrt(colSums(scale(X_train,TRUE,FALSE)^2))
X_train_std = scale(X_train,train_m,train_norm)
X_test = as.matrix(pro_test[,-9])
X_test_std = scale(X_test,train_m,train_norm)

lasso_in = seq(0,1,length=100)
set.seed(790)
lasso_cv = cv.lars(X_train_std,y_train,index=lasso_in,K=5,normalize = FALSE)


##(a)##

lasso_min = which.min(lasso_cv$cv)
best_min_frac = lasso_in[lasso_min]
fit_lasso = lars(X_train_std,y_train,type="lasso",normalize = FALSE)
best_min_coef = predict(fit_lasso,s=best_min_frac,type="coef",mode="frac")$coefficients
#Since X is centered, the estimated intercept is just the sample mean of y
int_coef = mean(y_train) 
#c(int_coef,best_min_coef)
lambda_search = function(x){
    sum(abs(coef(fit_lasso,s=x,mode="lambda")))-targetL1norm
}
targetL1norm = sum(abs(best_min_coef))
#By some trial and error I found that the target lambda range
#should be between 0.1 and 0.2
target_lambda_min = uniroot(lambda_search,c(0.1,0.2))$root
yhat_test_min = predict(fit_lasso,newx=X_test_std,s=best_min_frac,mode="frac")$fit
test_err_min = mean((y_test-yhat_test_min)^2)
cat('The best lambda is',target_lambda_min,'\n')
cat('The selected model and its estimated regression coefficients are','\n')
print(best_min_coef)
cat('TestErr =',test_err_min,'\n')

##(b)##

bound<-lasso_cv$cv[lasso_min]+lasso_cv$cv.error[lasso_min]
best_one_frac<-lasso_in[min(which(lasso_cv$cv<bound))]
best_one_coef <- coef(fit_lasso, s=best_one_frac, mode="frac")
#c(int_coef,best_one_coef)
targetL1norm = sum(abs(best_one_coef))
#Again, by some trial and error I found that the target lambda range
#should be between 1.2 and 1.3
target_lambda_one = uniroot(lambda_search,c(1.2,1.3))$root
yhat_test_one = predict(fit_lasso,newx=X_test_std,s=best_one_frac,mode="frac")$fit
test_err_one = mean((y_test-yhat_test_one)^2)  
cat('The best lambda is',target_lambda_one,'\n')
cat('The selected model and its estimated regression coefficients are','\n')
print(best_one_coef)
cat('TestErr =',test_err_one,'\n')

##(c)## Shooting Algorithm

ols_coef = lm(lpsa~.,data=data.frame(cbind(lpsa=y_train,X_train_std)))$coefficients
beta_in = ols_coef[-1]
s_fun = function(beta,j,X,y){
  xj = t(X[,j])
  s = xj%*%X[,-j]%*%(2*beta[-j])-2*xj%*%y
  return(s)
}
lambda = 10^seq(-2,2,length.out = 100)
beta_mat = matrix(0,nrow=length(lambda),ncol=p)
BIC_vec = rep(0,length(lambda))
colnames(beta_mat) = names(beta_in)
for(i in 1:length(lambda)){
  pre_L1norm = sum(abs(beta_in))
  beta_hat = beta_in
  counter = 0
  while(counter <= 5){
    for(j in 1:p){
      s0 = s_fun(beta_hat,j,X_train_std,y_train)
      beta_hat[j] = (s0 > lambda[i])*(lambda[i]-s0)/2 +
        (s0 < -lambda[i])*(-lambda[i]-s0)/2
    }
    cur_L1norm = sum(abs(beta_hat))
    if(abs(cur_L1norm-pre_L1norm)<1e-5){
      counter = counter + 1
    }
    pre_L1norm = cur_L1norm
  }
  yhat_train = X_train_std%*%beta_hat + int_coef
  BIC_vec[i] = n_train*log(mean((y_train-yhat_train)^2))+log(n_train)*(sum(beta_hat!=0)+1)
  beta_mat[i,] = beta_hat
}
best_BIC_loc = which.min(BIC_vec)
best_lambda_shoot = lambda[best_BIC_loc]
best_coef_shoot = beta_mat[best_BIC_loc,]
yhat_test = X_test_std%*%best_coef_shoot + int_coef
test_err = mean((y_test-yhat_test)^2)
cat('The best lambda is',best_lambda_shoot,'with a BIC of',round(BIC_vec[best_BIC_loc],4),'\n')
cat('The selected model and its estimated coefficients are','\n')
print(best_coef_shoot)
cat('TestErr =',test_err)

##(d)## Adaptive Lasso

w = abs(beta_in)
X_train_adapt = scale(X_train_std,FALSE,1/w)
fit_lasso_adapt = lars(X_train_adapt,y_train,type="lasso",normalize = FALSE)
index = seq(0,1,length.out = 100)
beta_adapt_list = vector('list',length(index))
BIC_adapt = {}
for(s in 1:length(index)){
  beta_adapt_scale = predict(fit_lasso_adapt,s=index[s],type="coef",mode="frac")$coefficients
  beta_adapt = beta_adapt_scale*w
  yhat_train_adapt = X_train_std%*%beta_adapt + int_coef
  train_err_adapt = mean((y_train-yhat_train_adapt)^2)
  beta_adapt_list[[s]] = beta_adapt
  BIC_adapt[s] = n_train*log(mean((y_train-yhat_train_adapt)^2))+log(n_train)*(sum(beta_adapt!=0)+1)
}
best_BIC_loc_adapt = which.min(BIC_adapt)
best_frac_adapt = index[best_BIC_loc_adapt]
best_coef_adapt = beta_adapt_list[[best_BIC_loc_adapt]]
yhat_test = X_test_std%*%best_coef_adapt + int_coef
test_err = mean((y_test-yhat_test)^2)
targetL1norm = sum(abs(best_coef_adapt))
target_lambda_adapt = round(uniroot(lambda_search,c(0.4,0.5))$root,4)
cat('The best lambda is',target_lambda_adapt,'with a BIC of',round(BIC_adapt[best_BIC_loc_adapt],4),'\n')
cat('The selected model and its estimated coefficients are','\n')
print(best_coef_adapt)
cat('TestErr =',test_err)