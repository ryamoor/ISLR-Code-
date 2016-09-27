

library(ISLR)
attach(Wage)


#Polynomials

#First we will use polynomials, and focus on a single predictor age:

fit<-lm(wage~poly(age, 4), data=Wage)
summary(fit)
#poly is generating orthogonal polynomials which means we can interpret
#this summary to mean that these are uncorrelated and can be interpreted
#seperately from each other in terms of significance 

#Plot the function
agelims<-range(age)
age.grid<-seq(from=agelims[1], to=agelims[2])
preds<-predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands<-cbind(preds$fit+2*preds$se, preds$fit-2*preds$se)
plot(age, wage, col="darkgrey")
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, col="blue", lty=2)

#I is to protect the ^2 and ^3 because the carrot means other things in the function
fita<-lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
summary(fita)

#plot fit of original and fita
plot(fitted(fit), fitted(fita))

#Using Anova to fit
fita<-lm(wage~education, data=Wage)
fitb<-lm(wage~education+age, data=Wage)
fitc<-lm(wage~education+poly(age,2), data=Wage)
fitd<-lm(wage~education+poly(age,3), data=Wage)
anova(fita, fitb,fitc,fitd)



#Polynomial logistic regression
fit<-glm(I(wage>250)~poly(age,3), data=Wage,family=binomial)
summary(fit)
preds<-predict(fit, list(age=age.grid), se=T)

#this formula is adding the fit to 0 (aka the predict) and to the upper and lower adjustment for the standard error bounds
se.bands<-preds$fit+cbind(fit=0, lower=-2*preds$se, upper=2*preds$se)
##checking them out
se.bands[1:5,]

#Now we use the inverse logit
prob.bands<-exp(se.bands)/(1+exp(se.bands))
matplot(age.grid, prob.bands, col="blue",lwd=c(2,1,1), lty=c(1,2,2),type="l",ylim=c(0,.1))
points(jitter(age), I(wage>250)/10, pch='|',cex=.5, col="black")


