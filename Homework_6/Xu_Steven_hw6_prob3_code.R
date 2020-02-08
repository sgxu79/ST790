library(foreach)
library(doParallel)

wd = "C:/Users/15306/Desktop/ST790/ZIP_data"
train_123 = do.call(rbind,lapply(2:3,function(i){
  cbind(i,read.table(file=paste(wd,"/train",i,".txt",sep=""),sep=","))
}
))
test_123 = read.table(file=paste(wd,"/zip_test.txt",sep=""))
test_123 = test_123[which(test_123[,1]%in%c(2,3)),]
colnames(test_123) = colnames(train_123)
train_123$i = factor(train_123$i)
test_123$i = factor(test_123$i)


cl = makeCluster(10)
registerDoParallel(cl)

#Grid searching for linear kernel

grid_lin = 2^seq(-8,6,length.out = 50)

lin_result = foreach(j = 1:length(grid_lin),.combine = 'c',.packages = c("e1071")) %dopar% {
  
  cost = grid_lin[j]
  
  lin_fit = svm(i~.,data = train_123,kernel = "linear",cost=cost)
  
  lin_pre = predict(lin_fit,newdata = test_123)
  
  lin_te = mean(lin_pre!=test_123$i)
  
  lin_te
  
}

grid_lin[which.min(lin_result)]

#Grid searchin for polynomial kernel

grid_poly = expand.grid(degree = 2:6, gamma = seq(0.001,1,length.out = 10),coef0 = seq(0,3,length.out = 10),cost = 2^seq(-6,3,length.out = 10))

poly_result = foreach(j = 1:nrow(grid_poly),.combine = 'c',.packages = c("e1071")) %dopar% {
  
  cost = grid_poly[j,]$cost
  
  gamma = grid_poly[j,]$gamma
  
  degree = grid_poly[j,]$degree
  
  coef0 = grid_poly[j,]$coef0
  
  poly_fit = svm(i~.,data = train_123,kernel = "polynomial",cost=cost, gamma=gamma, degree=degree, coef0=coef0)
  
  poly_pre = predict(poly_fit,newdata = test_123)
  
  poly_te = mean(poly_pre!=test_123$i)
  
  poly_te
  
}

grid_poly[which.min(poly_result),]

#Grid searching for Gaussian kernel

grid_rbf = expand.grid(gamma = 2^seq(-9,4,length.out = 20),cost = 2^seq(-8,6,length.out = 20))

rbf_result = foreach(j = 1:nrow(grid_rbf),.combine = 'c',.packages = c("e1071")) %dopar% {
  
  cost = grid_rbf[j,]$cost
  
  gamma = grid_rbf[j,]$gamma
  
  rbf_fit = svm(i~.,data = train_123,kernel = "radial",cost=cost, gamma=gamma)
  
  rbf_pre = predict(rbf_fit,newdata = test_123)
  
  rbf_te = mean(rbf_pre!=test_123$i)
  
  rbf_te
  
}

grid_rbf[which.min(rbf_result),]
