boundary = function(x){
  dnorm(x)/(0.65*dnorm(x,1,1)+0.35*dnorm(x,-1,2))-2/3
}
plot(seq(-10,10,0.1),boundary(seq(-10,10,0.1)),xlab="",ylab="",type="l")
abline(h=0,col=2,lty=2)
#By obersving the plot first root lies within (-5,0) and second (0,5)
b_point1  = uniroot(boundary,c(-5,0))$root
b_point2  = uniroot(boundary,c(0,5))$root
abline(v=b_point1,col=3,lty=3)
abline(v=b_point2,col=3,lty=3)
