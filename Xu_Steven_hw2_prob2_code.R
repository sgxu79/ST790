######################################################################################
# Beta is calculated using profile method with the form                              #
# Beta = [t(X)%*%diag(A-p(X))(I-Px)diag(A-p(x))X]^{-1}%*%[t(X)%*%diag(A-p(X))(I-Px)Y]#
######################################################################################

n = c(50,100,200,500)
m = 1000
beta = c(0.5,-1,1)
result = list(scenario.1=list(case.1={},case.2={}),scenario.2=list(case.1={},case.2={}))
for(i in 1:2){
  for(j in 1:2){
    result[[i]][[j]] = array(0,c(2,4,3),dimnames = list(c("mean","sd"),as.character(n),paste("beta.",1:3,sep="")))
  }
}

set.seed(790)

for(i in 1:4){
  nsamp = n[i]
  for(s in 1:2){
    for(c in 1:2){
      beta_mat = matrix(0,m,3)
      for(iter in 1:m){
        x1 = rbinom(nsamp,1,0.5)
        x2 = runif(nsamp,-1,1)
        prop.true = exp(x1-x2)/(1+exp(x1-x2))
        X_ori = cbind(1,x1,x2)
        ep = rnorm(nsamp,0,0.5)
        A = rbinom(nsamp,1,prop.true)
        if(s==1){
          h = 1+(0.5*x1+x2)^2
        }else{
          h = 1+0.5*x1+sin(pi*x2)
        }
        y = h + A*as.numeric(X_ori%*%beta)+ep
        if(c==1){
          prop = prop.true
        }else{
          prop = as.numeric(glm(A~X_ori,family = binomial(link = "logit"))$fitted)
        }
        Px = X_ori%*%solve(t(X_ori)%*%X_ori)%*%t(X_ori)
        X_new = (diag(nsamp)-Px)%*%diag(A-prop)%*%X_ori
        beta_mat[iter,] = solve(t(X_new)%*%X_new,t(X_new)%*%y)
      }
      beta.mean = colMeans(beta_mat)
      beta.sd = apply(beta_mat,2,sd)
      result[[s]][[c]][1,i,] = beta.mean
      result[[s]][[c]][2,i,] = beta.sd
    }
  }
}

############################################################
#Result is a list with hiearchy: Scenario --> Case --> beta#
############################################################

#result
