

library(class)
attach(Smarket)

xlag<-cbind(Lag1, Lag2)
train<-Year<2005

knn.pred<-knn(xlag[train,], xlag[!train,], Direction[train],k=3.5)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])






