load(file="~/hw4_train.rda")
load(file="~/hw4_test.rda")
lin_class = lm(y~x1+x2,data=dtrain)
coef = lin_class$coefficients
b_intercept_lin = (0.5-coef[1])/coef[3]
b_slope_lin = -coef[2]/coef[3]
abline(coef=c(b_intercept_lin,b_slope_lin))
lin_pred_train = ifelse(predict(lin_class,newdata=dtrain)>0.5,1,0)
result_train = table(dtrain$y,lin_pred_train)
result_train
lin_fp_train = result_train[1,2]/sum(result_train[1,])
lin_fn_train = result_train[2,1]/sum(result_train[2,])
lin_mc_train = (result_train[1,2]+result_train[2,1])/sum(result_train)
lin_pred_test = ifelse(predict(lin_class,newdata=dtest)>0.5,1,0)
result_test = table(dtest$y,lin_pred_test)
result_test
lin_fp_test = result_test[1,2]/sum(result_test[1,])
lin_fn_test = result_test[2,1]/sum(result_test[2,])
lin_mc_test = (result_test[1,2]+result_test[2,1])/sum(result_test)
legend("topleft",legend = c("Linear","Bayes"),col = c("black","blue"),lty=1:2)
