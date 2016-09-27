

library(ISLR)
library(tree)
attach(Carseats)

hist(Sales)
High<-ifelse(Sales<=8,"No","Yes")
Carseats<-data.frame(Carseats,High)

#plot the tree of High predicted by everything but sales
tree.carseats=tree(High~.-Sales,data=Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)

#prune the tree
set.seed(1011)
train<-sample(1:nrow(Carseats),250)
tree.carseats<-tree(High~.-Sales, Carseats, subset=train)

plot(tree.carseats);text(tree.carseats,pretty=0)
tree.pred<-predict(tree.carseats,Carseats[-train,],type="class")

with(Carseats[-train,], table(tree.pred, High))
(72+33)/150


#Prune the Tree and do 10 fold CV
cv.carseats<-cv.tree(tree.carseats, FUN=prune.misclass)
cv.carseats
#find that the size around 13 is best
plot(cv.carseats)

#Prune
prune.carseats<-prune.misclass(tree.carseats,best=13)
plot(prune.carseats);text(prune.carseats,pretty=0)


#use this pruned tree to predict
tree.pred<-predict(prune.carseats, Carseats[-train,],type='class')

#confusion matrix
with(Carseats[-train,],table(tree.pred,High))

#accuracy based on the confusion matrix above
(72+32)/150


