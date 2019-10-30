wd = getwd()
train_123 = do.call(rbind,lapply(1:3,function(i){
  cbind(i,read.table(file=paste(wd,"/train",i,".txt",sep=""),sep=","))
}
))
test_123 = read.table(file=paste(wd,"/zip_test.txt",sep=""))
test_123 = test_123[which(test_123[,1]%in%c(1,2,3)),]

k_vec = c(1,3,5,7,15)

train_label = factor(train_123[,1])
test_label = factor(test_123[,1])

error_mat = do.call(rbind,lapply(k_vec,function(k){
  train_fit = knn(train_123[,-1],train_123[,-1],train_label,k=k)
  test_fit = knn(train_123[,-1],test_123[,-1],train_label,k=k)
  train_err = mean(train_fit!=train_label)
  test_err = mean(test_fit!=test_label)
  return(c(train_err,test_err,k))
}))

colnames(error_mat) = c("TrEr","TeEr","k")
error_mat

#Remove V16
train_123 = train_123[,-17]
test_123 = test_123[,-17]
colnames(train_123) = c("label",paste("V",1:255,sep=""))
colnames(test_123) = c("label",paste("V",1:255,sep=""))
train_123 = as.data.frame(train_123)
test_123 = as.data.frame(test_123)
fit_lda = lda(label~.,data = train_123)
pre_train_lda = predict(fit_lda,train_123)$class
pre_test_lda = predict(fit_lda,test_123)$class
train_err_lda = mean(pre_train_lda!=train_label)
test_err_lda = mean(pre_test_lda!=test_label)
c(train_err_lda,test_err_lda)
