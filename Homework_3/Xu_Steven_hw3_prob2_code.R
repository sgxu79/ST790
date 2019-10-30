library(leaps)
prostate = read.table('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/prostate.data')
p = 8
pro_train = subset(prostate,train==TRUE)[,-10]
n_train = nrow(pro_train)
pro_test = subset(prostate,train==FALSE)[,-10]
n_test = nrow(pro_test)
fit_train = lm(lpsa~.,data=pro_train)

#summary(fit_train)

smry = summary(fit_train)
R2 = smry$r.squared
p_val = smry$coefficients[,4]
sig_coef = names(p_val[p_val<0.05])

#The significant variables are lcavol, lweight, lbph, svi

yhat_train = fit_train$fitted.values
yhat_test = predict(fit_train,newdata = pro_test)
y_train = pro_train[,9]
y_test = pro_test[,9]
train_err = mean((y_train-yhat_train)^2)
test_err = mean((y_test-yhat_test)^2)


reg_out = regsubsets(lpsa ~ ., data = pro_train, nbest = 1, method = "forward")

#summary(reg_out)
cat('The sets of regression coefficients for M1 - M8 along with the estimated TrainErr,BIC,AIC are','\n')

train_err_sub = {}
train_bic_sub = {}
train_aic_sub = {}
for(i in 1:8){
  cur_coef = coef(reg_out,i)
  cat(paste('M',i,sep=''),'\n')
  print(cur_coef)
  cur_X = cbind(1,as.matrix(pro_train[,names(cur_coef)[-1]]))
  cur_yhat = cur_X%*%cur_coef
  train_err_sub[i] = mean((y_train-cur_yhat)^2)
  train_bic_sub[i] = n_train*log(train_err_sub[i])+log(n_train)*ncol(cur_X)
  train_aic_sub[i] = n_train*log(train_err_sub[i])+2*ncol(cur_X)
  cat('\n')
  cat('TrainErr =',train_err_sub[i],'\n')
  cat('BIC =',train_bic_sub[i],'\n')
  cat('AIC =',train_aic_sub[i],'\n')
  cat('\n')
}
loc_best_bic = which.min(train_bic_sub)
best_coef_bic = coef(reg_out,loc_best_bic)
best_coef_name_bic = names(best_coef_bic)
best_X_bic = cbind(1,as.matrix(pro_test[,best_coef_name_bic[-1]]))
best_yhat_bic = best_X_bic%*%best_coef_bic
test_err_best_bic = mean((y_test-best_yhat_bic)^2)


loc_best_aic = which.min(train_aic_sub)
best_coef_aic = coef(reg_out,loc_best_aic)
best_coef_name_aic = names(best_coef_aic)
best_X_aic = cbind(1,as.matrix(pro_test[,best_coef_name_aic[-1]]))
best_yhat_aic = best_X_aic%*%best_coef_aic
test_err_best_aic = mean((y_test-best_yhat_aic)^2)
