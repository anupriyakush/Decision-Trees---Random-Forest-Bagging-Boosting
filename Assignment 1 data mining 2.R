library(MASS)
data(Boston)
set.seed(12825704)
index <- sample(nrow(Boston), nrow(Boston)*0.75)
boston.train <- Boston[index,]
boston.test <- Boston[-index,]
head(boston.test)
colnames(boston.test)


# LINEAR MODELS -----------------------------------------------------------


linear.boston <- lm(medv~crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+black, data=boston.train)
summary(linear.boston)
MSE.original.linear.train <- (summary(linear.boston)$sigma)^2
adjR.original.linear <- summary(linear.boston)$adj.r.squared
AIC(linear.boston)
BIC(linear.boston)
MSE.original.linear.train
adjR.original.linear
pi <- predict(linear.boston, newdata=boston.test)
MSPE.linear.test.original <- mean((pi-boston.test$medv)^2)
MSPE.linear.test.original


par(mfrow = c(2,2))
plot(linear.boston)
#Best subset regression
#install.packages('leaps')
library(leaps)
subset <- regsubsets(medv~., data=boston.train)
summary(subset)
plot(subset, scale = "bic")

model.best.subset <- lm(medv~chas+nox+rm+dis+ptratio+black+lstat, data=boston.train)
summary(model.best.subset)
MSE.bestsubset.linear.train <- (summary(model.best.subset)$sigma)^2
adjR.bestsubset.linear <- summary(model.best.subset)$adj.r.squared
AIC(model.best.subset)
BIC(model.best.subset)
MSE.bestsubset.linear.train
adjR.bestsubset.linear

pi <- predict(model.best.subset, newdata=boston.test)
MSPE.linear.test.bestsub <- mean((pi-boston.test$medv)^2)
MSPE.linear.test.bestsub
par(mfrow = c(2,2))
plot(model.best.subset)

#stepwise variable selection

nullmodel=lm(medv~1, data=boston.train)
fullmodel=lm(medv~., data=boston.train)
?step
model.step <- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "both")
summary(model.step)
MSE.model.step.train <- (summary(model.step)$sigma)^2
adjR.model.step.linear <- summary(model.step)$adj.r.squared
AIC(model.step)
BIC(model.step)
MSE.model.step.train
adjR.model.step.linear
par(mfrow=c(2,2))
plot(model.step)
#final model = model.step
#Out-of-sample prediction - test error 
pi <- predict(model.step, newdata=boston.test)
MSE.linear.test.step <- mean((pi-boston.test$medv)^2)
MSE.linear.test.step


# TREE --------------------------------------------------------------------
install.packages('rpart')
install.packages('rpart.plot') 
library(rpart)
library(rpart.plot)

original.tree <- rpart(formula = medv ~ ., data = boston.train)
prp(original.tree, extra = 1)
insample.prediction.original.tree <- predict(boston.rpart, boston.train)

outsample.prediction.original.tree <- predict(boston.rpart, boston.test)

summary(original.tree)
MSE.original.tree <- mean((boston.train$medv - insample.prediction.original.tree)^2)
MSE.original.tree
MSPE.original.tree <- mean((outsample.prediction.original.tree - boston.test$medv)^2)
MSPE.original.tree

plotcp(original.tree)
prune.original.tree <- prune(original.tree, cp = 0.03)
prp(prune.original.tree, extra = 1)


# Bagging -----------------------------------------------------------------


install.packages("ipred")
library(ipred)
boston.bag.tree<- bagging(medv~., data = boston.train, nbagg=100)
boston.bag.tree

#calculation number of bags
nbag <- seq(1,200,3)
MSPE.nbag <- rep(0,length(nbag))
for(i in 1:length(nbag)){
  boston.nbag <- bagging(medv~., data=boston.train, nbagg = nbag[i])
  boston.nbag.pred <- predict(boston.nbag, newdata=boston.test)
  MSPE.nbag[i] <- mean((boston.nbag.pred-boston.test$medv)^2)
}
plot(nbag, MSPE.nbag, type = 'l', col='blue', lwd=2, main = "MSPE vs number of trees")


#Prediction on testing sample.
bag.pred.train <- predict(boston.bag.tree, newdata = boston.train)
boston.bag.pred.test<- predict(boston.bag.tree, newdata = boston.test)
MSPE.bag <- mean((boston.test$medv -boston.bag.pred.test)^2)
MSE.bag <- mean((boston.train$medv -bag.pred.train)^2)
MSE.bag
MSPE.bag

  #out of bag sample
boston.bag.oob<- bagging(medv~., data = boston.train, coob=T, nbagg=110)
boston.bag.oob
#root mean squared error:  4.0735, MSE = 16 


# Random forest -----------------------------------------------------------
install.packages("randomForest")
library(randomForest)

rand.forest.tree <- randomForest(medv~., data = boston.train, importance = TRUE)
rand.forest.tree$importance
plot(rand.forest.tree$mse, type='l', col='blue', lwd=2)
rand.forest.tree.pred <- predict(rand.forest.tree, newdata = boston.test)
MSPE.rand.error <- mean((boston.test$medv - rand.forest.tree.pred)^2)
MSPE.rand.error


# Boosting ----------------------------------------------------------------
install.packages("gbm")
library(gbm)
boston.boosting.tree <- gbm(medv~., data = boston.train, distribution = "gaussian", n.trees = 1000)
summary(boston.boosting.tree)


plot(boston.boosting.tree, i="lstat")
plot(boston.boosting.tree, i="zn")

boston.boosting.tree.pred <- predict(boston.boosting.tree, newdata = boston.test, n.trees = 1000)
MSPE.boston.boosting.tree <- mean((boston.test$medv - boston.boosting.tree.pred)^2)
MSPE.boston.boosting.tree

ntree.boost <- seq(100,1000,100)
error <- rep(0,length(ntree.boost))
for (i in 1:length(ntree.boost)){
  pred.ntree.boost <- predict(boston.boosting.tree, newdata = boston.test, n.trees = ntree.boost[i])
  error[i] <- mean((boston.test$medv - pred.ntree.boost)^2)
}
plot(ntree.boost,error,type='l',lwd = 2, col='red')
