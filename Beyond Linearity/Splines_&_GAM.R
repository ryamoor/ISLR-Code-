


library(ISLR)
library(splines)
library(gam)

#We are effectively creating different cubic polynomials between each knot
#but keeping them continous at the knots to the second derivative
fit<-lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
plot(age, wage, col="darkgrey")
lines(age.grid, predict(fit, list(age=age.grid)), col="darkgreen", lwd=2)
abline(v=c(25,40,60), lty=2, col='darkgreen')
#These get rid of the wagging tails of polynomials

#Smoothing Splines(knots at every point)
fit<-smooth.spline(age, wage, df=16)
lines(fit, col="red",lwd=2)
##16 df is high and leaves a lot of wiggle


#LOOCV (leave one out cross validation is great for smoothing splines)

fit<-smooth.spline(age,wage,cv=TRUE)
lines(fit,col="purple",lwd=2)
fit


##GAM Generalized Additive Models
gam1<-gam(wage~s(age,df=4)+s(year,df=4)+education, data=Wage)
par(mfrow=c(1,3))
plot(gam1, se=T)

#gam with logistic regresion
gam2<-gam(I(wage>250)~s(age,df=4)+s(year,df=4)+education,data=Wage, family=binomial)
plot(gam2)

#linear year
gam2a<-gam(I(wage>250)~s(age,df=4)+year+education,data=Wage, family=binomial)

#anova the value .8242 determines you don't need to use a nonlinear term for year
anova(gam2a, gam2,test="Chisq")

par(mfrow=c(1,3))
lm1<-lm(wage~ns(age,df=4)+ns(year,df=4)+education,data=Wage)

plot.gam(lm1,se=T)
