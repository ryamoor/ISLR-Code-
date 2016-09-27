

library(ISLR)

Hitters<-na.omit(Hitters)

regfit.fwd<-regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
summary(regfit.fwd)


#plot (small is better for CP) Black means the variable is in, white is out.
plot(regfit.fwd, scale="Cp")

#Model Selection using a validation set
dim(Hitters)

set.seed(1)
train<-sample(seq(263), 180, replace=FALSE)
train

regfit.fwd<-regsubsets(Salary~., data=Hitters[train,], nvmax=19, method="forward")

#There is no predict function for regsubsets
#These lines of code will be to generate our predictions of test
val.errors<-rep(NA, 19)
x.test<-model.matrix(Salary~., data=Hitters[-train,])

for(i in 1:19){
        coefi<-coef(regfit.fwd,id=i)
        pred<-x.test[,names(coefi)]%*%coefi
        val.errors[i]<-mean((Hitters$Salary[-train]-pred)^2)
}


#plot the validation error
plot(sqrt(val.errors), ylab="Root MSE", ylim=c(300,400), pch=19, type="b")
points(sqrt(regfit.fwd$rss[-1]/180), col="blue", pch=19, type="b")
legend("topright", legend=c("Training", "Validation"), col=c("blue", "black"), pch=19)




#Function to Predict Regsubsets
predict.regsubsets<-function(object, newdata, id, ...){
        form<-as.formula(object$call[[2]])
        mat<-model.matrix(form, newdata)
        coefi<-coef(object, id=id)
        mat[, names(coefi)]%*%coefi
}