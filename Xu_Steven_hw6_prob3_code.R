train_123 = do.call(rbind,lapply(2:3,function(i){
  cbind(i,read.table(file=paste(wd,"/train",i,".txt",sep=""),sep=","))
}
))
test_123 = read.table(file=paste(wd,"/zip_test.txt",sep=""))
test_123 = test_123[which(test_123[,1]%in%c(2,3)),]