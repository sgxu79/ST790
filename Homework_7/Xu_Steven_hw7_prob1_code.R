library(tree)
library(foreach)
library(doParallel)
library(caret)

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


tree.fit = tree(i~.,data=train_123)
summary(tree.fit)
tree.pre = predict(tree.fit,newdata = test_123)
tree.pre = apply(tree.pre,1,which.max)
confusionMatrix(data = factor(tree.pre),reference = test_123$i)
mean(tree.pre!=test_123$i)


