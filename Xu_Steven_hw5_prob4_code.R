library(class)
wd = getwd()
load(paste(wd,"/hw5_train.rda",sep=""))
load(paste(wd,"/hw5_train.rda",sep=""))

k_vec = c(1,4,7,10,13,16,30,45,60,80,100,150,200)

error_mat = do.call(rbind,lapply(k_vec,function(k){
  train_fit = knn(dtrain[,-3],dtrain[,-3],factor(dtrain[,3]),k=k)
  test_fit = knn(dtrain[,-3],dtest[,-3],factor(dtrain[,3]),k=k)
  train_err = mean(train_fit!=factor(dtrain[,3]))
  test_err = mean(test_fit!=factor(dtest[,3]))
  return(c(train_err,test_err,nrow(dtrain)/k))
}))

colnames(error_mat) = c("TrEr","TeEr","df")
error_mat = as.data.frame(error_mat)

plot(error_mat$df,error_mat$TrEr,type="b",col="Red",
     ylim=c(min(error_mat[,-3]),max(error_mat[,-3])),
     xlab="N/k",ylab="Misclassification Error")
lines(error_mat$df,error_mat$TeEr,col="Green",type="b")
k_best = k_vec[which.min(error_mat$TeEr)]
abline(v = nrow(dtrain)/k_best,lty=2)

legend("topright",legend = c("Train","Test","k=30"),col = c("Red","Green","Black"),lty=c(1,1,2))

.pardefault <- par()
#Plot kNN decision boundary
rx1 <- range(dtest$x1)
rx2 <- range(dtest$x2)
# get lattice points in predictor space
px1 <- seq(from = rx1[1], to = rx1[2], by = 0.1 )
px2 <- seq(from = rx2[1], to = rx2[2], by = 0.1 )
xnew <- expand.grid(x1 = px1, x2 = px2)

# get the contour map
knn30 <- knn(train = dtrain[,-3], test = xnew, cl = dtrain[,3], k = 30, prob = TRUE)
prob <- attr(knn30, "prob")
prob <- ifelse(knn30=="1", prob, 1-prob)
prob30 <- matrix(prob, nrow = length(px1), ncol = length(px2))

# Figure 2.2
par(mar = rep(2,4))
contour(px1, px2, prob30, levels=0.5, labels="", xlab="", ylab="", main=
          "30-nearest neighbour", axes=FALSE)
points(dtest[,-3], col=ifelse(dtest[,3]==1, "coral", "cornflowerblue"))
points(xnew, pch=".", cex=2, col=ifelse(prob30>0.5, "coral", "cornflowerblue"))
box()
par(.pardefault)