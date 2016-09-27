



library(ISLR)
library(boot)
library(ggplot2)

plot(mpg~horsepower, data=Auto)

?cv.glm
##LOOCV
glm.fit<-glm(mpg~horsepower, data=Auto)

cv.glm(Auto, glm.fit)$delta #pretty slow(Doesn't use formula(5.2) on page 180)


## Lets write a simple function use formula (5.2)
loocv<-function(fit){
        #gives you the diagnoal hii
        h<-lm.influence(fit)$h
        mean((residuals(fit)/(1-h))^2)
}

##Now we try it out
loocv(glm.fit)

cv.error<-rep(0,5)
degree<-1:5
for(d in degree){
        glm.fit<-glm(mpg~poly(horsepower, d), data=Auto)
        cv.error[d]<-loocv(glm.fit)
}
plot(degree, cv.error, type="b")


## 10-Fold CV

cv.error10=rep(0,5)
for(d in degree){
        glm.fit<-glm(mpg~poly(horsepower,d), data=Auto)
        cv.error10[d]<-cv.glm(Auto, glm.fit, K=10)$delta[1]
}
lines(degree, cv.error10, type="b",col="red")
