library(MASS)
wd = getwd()
load(paste(wd,"/hw5_train.rda",sep=""))
load(paste(wd,"/hw5_train.rda",sep=""))
clas_err = function(ans,est){
  result = table(ans,est)
  b_fp = result[1,2]/sum(result[1,])
  b_fn = result[2,1]/sum(result[2,])
  b_mc = (result[1,2]+result[2,1])/sum(result)
  output = c(b_fp,b_fn,b_mc)
  names(output) = c("False Positive","False Negative","Classfication Error")
  return(output)
}

#Linear Classifier
lin_class = lm(y~x1+x2,data = dtrain)
coef = lin_class$coefficients
b_intercept_lin = (0.5-coef[1])/coef[3]
b_slope_lin = -coef[2]/coef[3]
plot(dtrain$x1,dtrain$x2,xlab="x1",ylab="x2")
points(train_g,col="green")
points(train_r,col="red")
abline(coef=c(b_intercept_lin,b_slope_lin))
lin_pred_train = ifelse(predict(lin_class,newdata=dtrain)>0.5,1,0)
lin_pred_test = ifelse(predict(lin_class,newdata=dtest)>0.5,1,0)
clas_err(dtrain$y,lin_pred_train)
clas_err(dtest$y,lin_pred_test)

#LDA
fit_lda = lda(y~x1+x2,data = dtrain)
pre_train_lda = predict(fit_lda,dtrain)$class
pre_test_lda = predict(fit_lda,dtest)$class
clas_err(dtrain$y,pre_train_lda)
clas_err(dtest$y,pre_test_lda)

#QDA
fit_qda = qda(y~x1+x2,data=dtrain)
pre_train_qda = predict(fit_qda,dtrain)$class
pre_test_qda = predict(fit_qda,dtest)$class
clas_err(dtrain$y,pre_train_qda)
clas_err(dtest$y,pre_test_qda)
