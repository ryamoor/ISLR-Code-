

library(ISLR)
library(leaps)

summary(Hitters)

Hitters<-na.omit(Hitters)
with(Hitters, sum(is.na(Salary)))



#Best Subset Regression
regfit.full<-regsubsets(Salary~.,data=Hitters, nvmax=19)
summary(regfit.full)
reg.summary<-summary(regfit.full)
names(reg.summary)


#Plot the output with CP vs variables
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], pch=20,col="red")


#plot (small is better for CP) Black means the variable is in, white is out.
plot(regfit.full, scale="Cp")
coef(regfit.full,10)



