diamond<-read.csv("~/Desktop/two_months_salary.csv", header=TRUE,stringsAsFactor=T)

require('caret')
require('nloptr')
require(ggplot2)

#SPLIT INTO TRAIN & TEST SETS
smp_size <- floor(0.70 * nrow(diamond))
set.seed(2013)
train_ind <- sample(seq_len(nrow(diamond)), size = smp_size)
train <- diamond[train_ind, ]
test <- diamond[-train_ind, ]
dim(train)
names(train)
train<-as.data.frame(train)
test<-as.data.frame(test)

logprice<-log(test$price)
test<-cbind(test,logprice)

#REDEFINE CATEGORICAL VARIABLES
mall<-as.matrix(train$channel=="Mall")
internet<-as.matrix(train$channel=="Internet")
indepedent<-as.matrix(train$channel=="Independent")
mall.test<-as.matrix(test$channel=="Mall")
internet.test<-as.matrix(test$channel=="Internet")
indepedent.test<-as.matrix(test$channel=="Independent")

not.ideal<-as.matrix(train$cut=="Not Ideal")
ideal<-as.matrix(train$cut=="Ideal")
not.ideal.test<-as.matrix(test$cut=="Not Ideal")
ideal.test<-as.matrix(test$cut=="Ideal")

goodmans<-as.matrix(train$store=="Goodmans")
chalmers<-as.matrix(train$store=="Chalmers")
fred<-as.matrix(train$store=="Fred Meyer")
R.holland<-as.matrix(train$store=="R. Holland")
ausmans<-as.matrix(train$store=="Ausmans")
university<-as.matrix(train$store=="University")
kay<-as.matrix(train$store=="Kay")
zales<-as.matrix(train$store=="Zales")
danford<-as.matrix(train$store=="Danford")
bluenile<-as.matrix(train$store=="Blue Nile")
ashford<-as.matrix(train$store=="Ashford")
riddles<-as.matrix(train$store=="Riddles")
goodmans.test<-as.matrix(test$store=="Goodmans")
chalmers.test<-as.matrix(test$store=="Chalmers")
fred.test<-as.matrix(test$store=="Fred Meyer")
R.holland.test<-as.matrix(test$store=="R. Holland")
ausmans.test<-as.matrix(test$store=="Ausmans")
university.test<-as.matrix(test$store=="University")
kay.test<-as.matrix(test$store=="Kay")
zales.test<-as.matrix(test$store=="Zales")
danford.test<-as.matrix(test$store=="Danford")
bluenile.test<-as.matrix(test$store=="Blue Nile")
ashford.test<-as.matrix(test$store=="Ashford")
riddles.test<-as.matrix(test$store=="Riddles")

#DESCRIPTIVE STATISTICS
require(pastecs)
stat.desc(train)

#FACTOR TRAIN CATEGORICAL VARIABLESD
train$cut<-factor(train$cut,c("Ideal","Not Ideal"))
train$channel<-factor(train$channel,c("Independent","Mall","Internet"))
train$store<-factor(train$store,c("Ashford","Ausmans","Blue Nile","Chalmers","Danford","Fred Meyer","Goodmans","Kay","R. Holland","Riddles","University","Zales"))

#FACTOR TEST CATEGORICAL VARIABLES
test$cut<-factor(test$cut,c("Ideal","Not Ideal"))
test$channel<-factor(test$channel,c("Independent","Mall","Internet"))
test$store<-factor(test$store,c("Ashford","Ausmans","Blue Nile","Chalmers","Danford","Fred Meyer","Goodmans","Kay","R. Holland","Riddles","University","Zales"))

#HISTORGRAMS
require(lattice)
hist(train$carat,prob=T)
curve(dnorm(x,mean=mean(train$carat),sd=sd(train$carat)),add=T,col="blue")
abline(v=mean(train$carat),col="red")
hist(train$color,prob=T)
curve(dnorm(x,mean=mean(train$color),sd=sd(train$color)),add=T,col="blue")
abline(v=mean(train$color),col="red")
hist(train$clarity,prob=T)
curve(dnorm(x,mean=mean(train$clarity),sd=sd(train$clarity)),add=T,col="blue")
abline(v=mean(train$clarity),col="red")
hist(train$price,prob=T)
curve(dnorm(x,mean=mean(train$price),sd=sd(train$price)),add=T,col="blue")
abline(v=mean(train$price),col="red")
barplot(prop.table(table(train$cut)),main="Frequency of Cut")
barplot(prop.table(table(train$channel)),main="Frequency of Channel")
barplot(prop.table(table(train$store)),main="Frequency of Store")

#Q-Q PLOT MATHS
n.carat=length(train$carat)
probabilities.carat=(1:n.carat)/(n.carat+1)
normal.quantiles.carat=qnorm(probabilities.carat,mean(train$carat,na.rm=T),sd(train$carat,na.rm=T))
n.price=length(train$price)
probabilities.price=(1:n.price)/(n.price+1)
normal.quantiles.price=qnorm(probabilities.price,mean(train$price,na.rm=T),sd(train$price,na.rm=T))

#Q-Q PLOTS
plot(sort(normal.quantiles.carat),sort(train$carat),xlab='Theoretical Quantiles from Normal Distribution',ylab='Sample Quantiles from Carat',main='Q-Q Plot Carat')
abline(0,1)
plot(sort(normal.quantiles.price),sort(train$price),xlab='Theoretical Quantiles from Normal Distribution',ylab='Sample Quantiles from Price',main='Q-Q Plot Price')
abline(0,1)

#HISTOGRAM OF LOGPRICE
logprice<-log(train$price)
n.logprice=length(logprice)
probabilities.logprice=(1:n.logprice)/(n.logprice+1)
normal.quantiles.logprice=qnorm(probabilities.logprice,mean(logprice,na.rm=T),sd(logprice,na.rm=T))
plot(sort(normal.quantiles.logprice),sort(logprice),xlab='Theoretical Quantiles from Normal Distribution',ylab='Sample Quantiles from Log Price',main='Q-Q Plot Log Price')
abline(0,1)
hist(logprice,prob=T)
curve(dnorm(x,mean=mean(logprice),sd=sd(logprice)),add=T,col="blue")
abline(v=mean(logprice),col="red")

#BOXPLOTS OF CONTINUOUS VARIABLES
plot1<-boxplot(train$carat,notch=F,outline=T,plot=T,col="green",ylab="Carat",xlab="Diamond Size",main="Diamond Carat Boxplot")
plot2<-boxplot(train$price,notch=F,outline=T,plot=T,col="blue",ylab="Price",xlab="Diamond Price",main="Diamond Price Boxplot")
plot3<-boxplot(logprice,notch=F,outline=T,plot=T,col="red",ylab="Log Price",xlab="Log of Diamond Price",main="Log of Diamond Price Boxplot")

#CORRELATIONS
cor(train[sapply(train,is.numeric)],use="complete.obs")
require(corrplot)
corrplot(cor(train[sapply(train,is.numeric)],use="complete.obs"),type="lower",method="ellipse")
require(psych)
corr.test(train[sapply(train,is.numeric)],use="complete.obs")

#INITIAL TREE MODEL
require(tree)
tree.data<-tree(train$price~.,train)
summary(tree.data)
plot(tree.data)
text(tree.data,pretty=2,cex=0.8,col="red")

#NAIVE MODEL FOR TRAIN$PRICE, TRAIN$PRICE -COLOR, LOGPRICE
naive.model<-lm(train$price~.,data=train)
summary(naive.model)
extractAIC(naive.model)
plot(resid(naive.model),ylab="Residuals",main="Naive Model")
abline(0,0,col="red")
yhat.nm=predict(naive.model,newdata=test)
nm.test=test[,"price"]
mean((yhat.nm-nm.test)^2)
dat.nm<-data.frame(x=nm.test,y=yhat.nm)
res.nm<-stack(data.frame(Observed=dat.nm$x,Predicted=yhat.nm))
res.nm<-cbind(res.nm,x=dat.nm$x,2)
require(lattice)
xyplot(values~x,data=res.nm,group=ind,auto.key=FALSE,main="Naive: Predicted v. Observed")

naive.model1<-lm(train$price~carat+clarity+channel+store, data=train)
summary(naive.model1)
extractAIC(naive.model1)
plot(resid(naive.model1),ylab="Residuals",main="Naive Model (-Color)")
abline(0,0,col="red")
yhat.nm1=predict(naive.model1,newdata=test)
nm.test1=test[,"price"]
mean((yhat.nm1-nm.test1)^2)
dat.nm1<-data.frame(x=nm.test1,y=yhat.nm1)
res.nm1<-stack(data.frame(Observed=dat.nm1$x,Predicted=yhat.nm1))
res.nm1<-cbind(res.nm1,x=dat.nm1$x,2)
require(lattice)
xyplot(values~x,data=res.nm1,group=ind,auto.key=FALSE,main="Naive (-Color): Predicted v. Observed")


naive.model1<-lm(train$price~tcarat+clarity+channel+store, data=train)
summary(naive.model1)
extractAIC(naive.model1)
plot(resid(naive.model1),ylab="Residuals",main="Naive Model (-Color)")
abline(0,0,col="red")
yhat.nm1=predict(naive.model1,newdata=test)
nm.test1=test[,"price"]
mean((yhat.nm1-nm.test1)^2)
dat.nm1<-data.frame(x=nm.test1,y=yhat.nm1)
res.nm1<-stack(data.frame(Observed=dat.nm1$x,Predicted=yhat.nm1))
res.nm1<-cbind(res.nm1,x=dat.nm1$x,2)
require(lattice)
xyplot(values~x,data=res.nm1,group=ind,auto.key=FALSE,main="Naive (-Color): Predicted v. Observed")


naive.model2<-lm(logprice~carat+color+clarity+channel+store+cut,data=train)
summary(naive.model2)
extractAIC(naive.model2)
plot(resid(naive.model2),ylab="Residuals",main="Naive Model (logprice)")
abline(0,0, col="red")
yhat.nm2=predict(naive.model2,newdata=test)
nm.test2=test[,"logprice"]
mean((yhat.nm2-nm.test2)^2)
dat.nm2<-data.frame(x=nm.test2,y=yhat.nm2)
res.nm2<-stack(data.frame(Observed=dat.nm2$x,Predicted=yhat.nm2))
res.nm2<-cbind(res.nm2,x=dat.nm2$x,2)
require(lattice)
xyplot(values~x,data=res.nm2,group=ind,auto.key=FALSE,main="Naive (Log Price): Predicted v. Observed")



#FORWARD BACKWARD AND STEPWISE FOR TRAIN$PRICE
require(MASS)
forward<-lm(price~carat+color+clarity+mall+internet+ideal+not.ideal+goodmans+chalmers+fred+R.holland+ausmans+university+kay+zales+danford+bluenile+ashford+riddles,data=train)
forward.null<-lm(price~1,data=train)
forward<-step(forward.null,scope=list(lower=forward.null,upper=forward),direction="forward")
step(forward,data=train,direction="backward")
step(forward.null, scope=list(upper=forward), data=train,direction="both")


#FORWARD BACKWARD AND STEPWISE FOR LOGPRICE
forward.log<-lm(logprice~carat+color+clarity+channel+store+cut,data=train)
forward.log.null<-lm(logprice~1,data=train)
step(forward.log.null,scope=list(lower=forward.log.null,upper=forward.log),direction="forward")
step(forward.log,data=train,direction="backward")
step(forward.log.null, scope=list(upper=forward.log), data=train,direction="both")

yhat.for=predict(step(forward.log.null,scope=list(lower=forward.log.null,upper=forward.log),direction="forward"),newdata=test)
for.test=test[,"logprice"]
mean((yhat.for-for.test)^2)
dat.for<-data.frame(x=for.test,y=yhat.for)
res.for<-stack(data.frame(Observed=dat.for$x,Predicted=yhat.for))
res.for<-cbind(res.for,x=dat.for$x,2)
require(lattice)
xyplot(values~x,data=res.for,group=ind,auto.key=FALSE,main="Forward (Log Price): Predicted v. Observed")

yhat.bac=predict(step(forward.log,data=train,direction="backward"),newdata=test)
bac.test=test[,"logprice"]
mean((yhat.bac-bac.test)^2)
dat.bac<-data.frame(x=bac.test,y=yhat.bac)
res.bac<-stack(data.frame(Observed=dat.bac$x,Predicted=yhat.bac))
res.bac<-cbind(res.bac,x=dat.bac$x,2)
require(lattice)
xyplot(values~x,data=res.bac,group=ind,auto.key=FALSE,main="Backward (Log Price): Predicted v. Observed")

yhat.step=predict(step(forward.log.null, scope=list(upper=forward.log), data=train,direction="both"),newdata=test)
step.test=test[,"logprice"]
mean((yhat.step-step.test)^2)
dat.step<-data.frame(x=step.test,y=yhat.step)
res.step<-stack(data.frame(Observed=dat.step$x,Predicted=yhat.step))
res.step<-cbind(res.step,x=dat.step$x,2)
require(lattice)
xyplot(values~x,data=res.step,group=ind,auto.key=FALSE,main="Stepwise (Log Price): Predicted v. Observed")


require(leaps)
leaps<-regsubsets(train$price~.,data=train,nbest=10)
leaps
plot(leaps,scale="adjr2")
plot(leaps,scale="bic")
leaps.model<-leaps.model<-lm(price~carat+color+clarity+not.ideal+mall+fred+goodmans+R.holland,data=train)
summary(leaps.model)
plot(resid(leaps.model),ylab="Residuals",main="Leaps Model")
abline(0,0, col="red")
extractAIC(leaps.model)
leaps.model.test<-lm(price~carat+color+clarity+not.ideal.test+mall.test+fred.test+goodmans.test+R.holland.test, data=test)
yhat.leaps=predict(leaps.model.test,newdata=test)
leaps.test2=test[,"price"]
mean((yhat.leaps-leaps.test2)^2)
dat.leaps<-data.frame(x=leaps.test2,y=yhat.leaps)
res.leaps<-stack(data.frame(Observed=dat.leaps$x,Predicted=yhat.leaps))
res.leaps<-cbind(res.leaps,x=dat.leaps$x,2)
require(lattice)
xyplot(values~x,data=res.leaps,group=ind,auto.key=FALSE,main="Leaps: Predicted v. Observed")




#LASSO MODEL
require(glmnet)
x<-model.matrix(price~.,train)[,-1]
y<-train$price
grid<-10^seq(10,-2,length=25)
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
lasso.train=sample(1:nrow(x),nrow(x)/2)
lasso.test=(-train)
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:18,]
lasso.coef
lasso.coef[lasso.coef!=0]

#LINEAR MODEL FOR TRAIN$PRICE AND LOGPRICE
linear.model<-lm(train$price~train$carat+train$color+train$clarity+mall++ideal+goodmans+fred+R.holland,data=train)
summary(linear.model)
extractAIC(linear.model)
BIC(linear.model)
linear.model.test<-lm(price~carat+color+clarity+mall.test++ideal.test+goodmans.test+fred.test+R.holland.test,data=test)
yhat.lin=predict(linear.model.test,newdata=test)
lin.test2=test[,"price"]
mean((yhat.lin-lin.test2)^2)
dat.lin<-data.frame(x=lin.test2,y=yhat.lin)
res.lin<-stack(data.frame(Observed=dat.lin$x,Predicted=yhat.lin))
res.lin<-cbind(res.lin,x=dat.lin$x,2)
require(lattice)
xyplot(values~x,data=res.lin,group=ind,auto.key=FALSE,main="Analyst Linear: Predicted v. Observed")


linear.logmodel<-lm(logprice~train$carat+train$color+train$clarity+goodmans+R.holland+kay+zales,data=train)
summary(linear.logmodel)
extractAIC(linear.logmodel)
BIC(linear.logmodel)
linear.logmodel.test<-lm(logprice~carat+color+clarity+mall.test++ideal.test+goodmans.test+fred.test+R.holland.test,data=test)
yhat.log=predict(linear.logmodel.test,newdata=test)
log.test2=test[,"logprice"]
mean((yhat.log-log.test2)^2)
dat.log<-data.frame(x=log.test2,y=yhat.log)
res.log<-stack(data.frame(Observed=dat.log$x,Predicted=yhat.log))
res.log<-cbind(res.log,x=dat.log$x,2)
require(lattice)
xyplot(values~x,data=res.log,group=ind,auto.key=FALSE,main="Analyst Linear (Log Price): Predicted v. Observed")



#LINEAR PLUS INTERACTION FOR TRAIN$PRICE AND LOGPRICE
linear.plus<-lm(train$price~train$carat+mall+goodmans+kay+train$cut+train$carat:train$color+train$carat:train$clarity,data=train)
summary(linear.plus)
extractAIC(linear.plus)
BIC(linear.plus)
linear.plus.test<-lm(price~carat+mall.test+goodmans.test+kay.test+cut+carat:color+carat:clarity,data=test)
yhat.lp=predict(linear.plus.test,newdata=test)
lp.test2=test[,"price"]
mean((yhat.lp-lp.test2)^2)
dat.lp<-data.frame(x=lp.test2,y=yhat.lp)
res.lp<-stack(data.frame(Observed=dat.lp$x,Predicted=yhat.lp))
res.lp<-cbind(res.lp,x=dat.lp$x,2)
require(lattice)
xyplot(values~x,data=res.lp,group=ind,auto.key=FALSE,main="Analyst Linear+Interaction Price: Predicted v. Observed")


linear.logplus<-lm(logprice~train$carat+mall+goodmans+train$carat:train$color+train$carat:train$clarity,data=train)
summary(linear.logplus)
extractAIC(linear.logplus)
BIC(linear.logplus)
linear.pluslog.test<-lm(logprice~carat+mall.test+goodmans.test+kay.test+cut+carat:color+carat:clarity,data=test)
yhat.lpp=predict(linear.pluslog.test,newdata=test)
lpp.test2=test[,"logprice"]
mean((yhat.lpp-lpp.test2)^2)
dat.lpp<-data.frame(x=lpp.test2,y=yhat.lpp)
res.lpp<-stack(data.frame(Observed=dat.lpp$x,Predicted=yhat.lpp))
res.lpp<-cbind(res.lpp,x=dat.lpp$x,2)
require(lattice)
xyplot(values~x,data=res.lpp,group=ind,auto.key=FALSE,main="Analyst Linear+Interaction (Log Price): Predicted v. Observed")



#TREE MODEL
tree.model<-tree(train$price~train$carat+train$color+train$clarity+mall+internet+goodmans+chalmers+fred+R.holland+ausmans+university+kay+zales+danford+bluenile+ashford+riddles,data=train)
summary(tree.model)
plot(tree.model)
text(tree.model,pretty=2,cex=0.8,col="red")
tree.model.test<-tree(price~carat+color+clarity+mall.test+internet.test+goodmans.test+chalmers.test+fred.test+R.holland.test+ausmans.test+university.test+kay.test+zales.test+danford.test+bluenile.test+ashford.test+riddles.test,data=test)
yhat.tree=predict(tree.model.test,newdata=test)
tree.test2=test[,"price"]
mean((yhat.tree-tree.test2)^2)



#RANDOM FOREST CODE
require(randomForest)
smp_size.r <- floor(0.50 * nrow(train))
set.seed(2013)
train.r <- sample(seq_len(nrow(train)), size = smp_size.r)
ran.train <- train[train.r, ]
ran.test<-train[-train.r,]
ran.train$cut<-as.numeric(ran.train$cut)
ran.train$store<-as.numeric(ran.train$store)
ran.train$channel<-as.numeric(ran.train$channel)
ran.test$cut<-as.numeric(ran.test$cut)
ran.test$store<-as.numeric(ran.test$store)
ran.test$channel<-as.numeric(ran.test$channel)
rf.train<-randomForest(ran.train$price~.,data=ran.train,mtry=6,importance=T)
yhat.rf=predict(rf.train,newdata=ran.test)
tree.test=ran.test[,"price"]
mean((yhat.rf-tree.test)^2)
varImpPlot(rf.train)



