
library(ISLR)

##Bootstrap
##Minimum risk Investment - Section 5.2

alpha<-function(x,y){
        vx<-var(x)
        vy<-var(y)
        cxy<-cov(x,y)
        (vy-cxy)/(vx+vy-2*cxy)
}

alpha(Portfolio$X, Portfolio$Y)

##What is the Standard error of alpha?

alpha.fn<-function(data, index){
        with(data[index, ],alpha(X,Y))
        
}

alpha.fn(Portfolio,1:100)

#Bootstrap
set.seed(1)
alpha.fn(Portfolio, sample(1:100, 100, replace=TRUE))

boot.out<-boot(Portfolio, alpha.fn, R=1000)
boot.out

plot(boot.out)





