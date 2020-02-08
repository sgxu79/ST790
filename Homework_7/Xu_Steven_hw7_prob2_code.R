library(earth)

wd = "C:/Users/15306/Desktop/ST790/ZIP_data"
train_123 = do.call(rbind,lapply(1:3,function(i){
  cbind(i,read.table(file=paste(wd,"/train",i,".txt",sep=""),sep=","))
}
))
test_123 = read.table(file=paste(wd,"/zip_test.txt",sep=""))
test_123 = test_123[which(test_123[,1]%in%c(1:3)),]
colnames(test_123) = colnames(train_123)
train_123$i = factor(train_123$i)
test_123$i = factor(test_123$i)


mars.fit = earth(i~.,degree = 2,nprune = 30, data=train_123)
mars.pre_tr = apply(mars.fit$fitted.values,1,which.max)
mean(mars.pre_tr!=train_123$i)
mars.pre = predict(mars.fit, newdata = test_123, type="response")
mars.pre = apply(mars.pre,1,which.max)
confusionMatrix(data = factor(mars.pre),reference = test_123$i)
mean(mars.pre!=test_123$i)


