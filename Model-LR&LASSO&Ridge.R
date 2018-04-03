load("data_model.rda")

attach(data)

summary(data)
str(data)

##1. Linear Regression


data1 = data[,-c(1,23)]
set.seed(1)
train = sample(nrow(data1), nrow(data1)*(7/10))
test =(1:nrow(data1))[-train]
testdata=data1[test,]
traindata=data1[train,]

LR1 = lm(ADJ.ITEM96~., data = data1, subset = train)
LR = lm(ADJ.ITEM96~.-FID-ITEM44B,data = data,subset = train)

LR.mse.train=mean((traindata$ADJ.ITEM96-predict(LR1, data1[train,]))^2)
LR.mse.test=mean((testdata$ADJ.ITEM96-predict(LR1, testdata))^2)
summary(LR1)

#LR1$xlevels[["ITEM44B"]] <- union(LR1$xlevels[["ITEM44B"]], levels(testdata$ITEM44B))
mean((testdata$ADJ.ITEM96-predict(LR1, data1[test,]))^2)



summary(LR)


mean((testdata$ADJ.ITEM96-predict(LR,testdata))^2)

max(testdata$ADJ.ITEM96-predict(LR,testdata))

LR_pred=predict(LR1,newdata=testdata,type = "response")
LR_train=predict(LR1,newdata=traindata,type = "response")

par(mfrow=c(2,2))
plot(LR1)



LR.pred.data=data.frame(LR_pred)
LR.train.data=data.frame(LR_train)



##2.LASSO
library(glmnet)
mtx=model.matrix(ADJ.ITEM96~.,data = data1)[,-1]
model.lasso=glmnet(mtx[train,],traindata$ADJ.ITEM96,alpha=1)
set.seed(1)
cv.out=cv.glmnet(mtx[train,],traindata$ADJ.ITEM96,alpha=1)
best.lambda=cv.out$lambda.min
best.lambda

par(mfrow=c(1,1))
plot(cv.out)

lasso.coef=predict(model.lasso,s=best.lambda,type="coefficients")
lasso.pred=predict(model.lasso,s=best.lambda,newx = mtx[test,])
lasso.train=predict(model.lasso,s=best.lambda,newx = mtx[train,])

lasso.mse.test=mean((testdata$ADJ.ITEM96-lasso.pred)^2)
lasso.mse.train=mean((traindata$ADJ.ITEM96-lasso.train)^2)
lasso.coef

lasso.pred.data=data.frame(lasso.pred)
lasso.train.data=data.frame(lasso.train)


##3.Ridge

model.ridge=glmnet(mtx[train,],traindata$ADJ.ITEM96,alpha=0)
set.seed(1)
cv.out.ridge=cv.glmnet(mtx[train,],traindata$ADJ.ITEM96,alpha=0)
best.lambda.r=cv.out.ridge$lambda.min
best.lambda.r
ridge.coef=predict(model.ridge,s=best.lambda.r,type="coefficients")
ridge.pred=predict(model.ridge,s=best.lambda.r,newx = mtx[test,])
ridge.train=predict(model.ridge,s=best.lambda.r,newx = mtx[train,])

ridge.mse.test=mean((testdata$ADJ.ITEM96-ridge.pred)^2)
ridge.mse.train=mean((traindata$ADJ.ITEM96-ridge.train)^2)
ridge.coef

ridge.pred.data=data.frame(ridge.pred)
ridge.train.data=data.frame(ridge.train)

train.mse=c(LR.mse.train,lasso.mse.train,ridge.mse.train)
test.mse=c(LR.mse.test,lasso.mse.test,ridge.mse.test)


##test

test.result=cbind(testdata[,c(1,59)],LR.pred.data[,1],lasso.pred.data[,1],ridge.pred.data[,1])
colnames(test.result)[2:5]=c("Test.ITEM96","Linear","Lasso","Ridge")
library(dplyr)
test.error=test.result%>%
  mutate(Linear.Err=Linear-Test.ITEM96,
         Lasso.Err=Lasso-Test.ITEM96,
         Ridge.Err=Ridge-Test.ITEM96,
         Linear.Err.pc=(Linear-Test.ITEM96)/Test.ITEM96,
         Lasso.Err.pc=(Lasso-Test.ITEM96)/Test.ITEM96,
         Ridge.Err.pc=(Ridge-Test.ITEM96)/Test.ITEM96,
         Linear.Dir=as.factor(ifelse(Linear>Test.ITEM96,"Above","Below")),
         Lasso.Dir=as.factor(ifelse(Lasso>Test.ITEM96,"Above","Below")),
         Ridge.Dir=as.factor(ifelse(Ridge>Test.ITEM96,"Above","Below")))


summary(test.error)

###Test error plot
par(mfrow=c(3,1))
plot(x=test.error$Test.ITEM96,y=test.error$Linear.Err,main = "Linear Residual")
plot(x=test.error$Test.ITEM96,y=test.error$Lasso.Err,main = "Lasso Residual")
plot(x=test.error$Test.ITEM96,y=test.error$Ridge.Err,main = "Ridge Residual")

###Percentage plot
par(mfrow=c(3,1))
plot(x=test.error$Test.ITEM96,y=test.error$Linear.Err.pc,main = "Linear Residual%")
plot(x=test.error$Test.ITEM96,y=test.error$Lasso.Err.pc,main = "Lasso Residual%")
plot(x=test.error$Test.ITEM96,y=test.error$Ridge.Err.pc,main = "Ridge Residual%")



##Training Error
train.result=cbind(data[train,c(1,59)],LR.train.data[,1],lasso.train.data[,1],ridge.train.data[,1])
colnames(train.result)[2:5]=c("Train.ITEM96","Linear","Lasso","Ridge")

train.error=train.result%>%
  mutate(Linear.Err=Linear-Train.ITEM96,
         Lasso.Err=Lasso-Train.ITEM96,
         Ridge.Err=Ridge-Train.ITEM96,
         Linear.Err.pc=(Linear-Train.ITEM96)/Train.ITEM96,
         Lasso.Err.pc=(Lasso-Train.ITEM96)/Train.ITEM96,
         Ridge.Err.pc=(Ridge-Train.ITEM96)/Train.ITEM96,
         Linear.Dir=as.factor(ifelse(Linear>Train.ITEM96,"Above","Below")),
         Lasso.Dir=as.factor(ifelse(Lasso>Train.ITEM96,"Above","Below")),
         Ridge.Dir=as.factor(ifelse(Ridge>Train.ITEM96,"Above","Below")))


###Train error plot
par(mfrow=c(3,1))
plot(x=train.error$Train.ITEM96,y=train.error$Linear.Err,main = "Linear Residual (Train)")
plot(x=train.error$Train.ITEM96,y=train.error$Lasso.Err,main = "Lasso Residual (Train)")
plot(x=train.error$Train.ITEM96,y=train.error$Ridge.Err,main = "Ridge Residual (Train)")

###Percentage plot
par(mfrow=c(3,1))
plot(x=train.error$Train.ITEM96,y=train.error$Linear.Err.pc,main = "Linear Residual% (Train)")
plot(x=train.error$Train.ITEM96,y=train.error$Lasso.Err.pc,main = "Lasso Residual% (Train)")
plot(x=train.error$Train.ITEM96,y=train.error$Ridge.Err.pc,main = "Ridge Residual% (Train)")










