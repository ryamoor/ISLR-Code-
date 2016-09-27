
library(ISLR)

names(Smarket)
summary(Smarket)

#scatterplot matrix
pairs(Smarket, col=Smarket$Direction)

#Logistic Regresssion
glm.fit<-glm(data=Smarket,Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial)

summary(glm.fit)
glm.probs<-predict(glm.fit, type="response")
glm.probs[1:5]
glm.pred<-ifelse(glm.probs>.5, "Up","Down")
attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred==Direction)


#Make Training and test set
train<-Year<2005
glm.fit<-glm(data=Smarket,Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial, subset=train)

glm.probs<-predict(glm.fit, newdata=Smarket[!train,], type="response")
glm.pred<-ifelse(glm.probs>0.5, "Up","Down")
Direction.2005<-Smarket$Direction[!train]

table(glm.pred,Direction.2005)


#Smaller Model
glm.fit<-glm(data=Smarket,Direction~Lag1+Lag2, family=binomial, subset=train)

glm.probs<-predict(glm.fit, newdata=Smarket[!train,], type="response")
glm.pred<-ifelse(glm.probs>0.5, "Up","Down")
Direction.2005<-Smarket$Direction[!train]

table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
