

library(glmnet)
Hitters<-na.omit(Hitters)
x<-model.matrix(Salary~. - 1, data=Hitters)
y<-Hitters$Salary

#Ridge Regression
fit.ridge<-glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda", label=TRUE)
cv.ridge<-cv.glmnet(x,y,alpha=0)
plot(cv.ridge)


#Lasso
fit.lasso<-glmnet(x,y)
#this can also be used with "dev" in xvar
plot(fit.lasso,xvar="lambda", label=TRUE)
cv.lasso<-cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)


#Validation
set.seed(1)
train<-sample(seq(263), 180, replace=FALSE)

lasso.train<-glmnet(x[train,],y[train])
lasso.train

pred<-predict(lasso.train, x[-train,])
dim(pred)

#apply over each column and take the mean then sqrt
rmse<-sqrt(apply((y[-train]-pred)^2,2,mean))
plot(log(lasso.train$lambda),rmse, type="b",xlab="Log(lambda)")
lam.best<-lasso.train$lambda[order(rmse)[1]]
lam.best
coef(lasso.train, s=lam.best)
