

library(randomForest)
library(gbm)
library(MASS)


set.seed(101)
dim(Boston)

#create training set
train<-sample(1:nrow(Boston),300)

#Random Forest (OOB mean squared residuals)
rf.boston<-randomForest(medv~., data=Boston, subset=train)
rf.boston

#tuning parameters
oob.err<-double(13)
test.err<-double(13)

for(mtry in 1:13){
        fit<-randomForest(medv~., data=Boston,subset=train, mtry=mtry, ntree=400)
        oob.err[mtry]<-fit$mse[400]
        pred<-predict(fit,Boston[-train,])
        test.err[mtry]<-with(Boston[-train,],mean((medv-pred)^2))
        cat(mtry," ")
        
}

# 4-8 seems best depth
matplot(1:mtry, cbind(test.err, oob.err), pch=19, col=c('red','blue'), type="b", ylab="Mean Squared Error")
legend("topright", legend=c("OOB", "Test"), pch=19,col=c("red","blue"))


#Boosting
boost.boston<-gbm(medv~., data=Boston[train, ], distribution = "gaussian", n.trees=10000, shrinkage=.01, interaction.depth = 4)

summary(boost.boston)

#plot the top two variables
plot(boost.boston, i="lstat")
plot(boost.boston, i="rm")

#Cross validation is need to determine what parameters should be used in GBM
n.trees<-seq(from=100, to=10000, by=100)
predmat<-predict(boost.boston, newdata=Boston[-train,], n.trees=n.trees)
dim(predmat)

#recycle medv with apply and calculate MSE
berr<-with(Boston[-train,], apply((predmat-medv)^2, 2, mean))

plot(n.trees, berr, pch=19, ylab="Mean Squared Error", xlab="# Trees", main="Boosting Test Error")

#show the level of performance from test error from random forest
abline(h=min(test.err), col="red")
