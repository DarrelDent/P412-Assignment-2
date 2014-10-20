setwd("C:\\Users\\abo586\\Documents\\Dropbox\\PREDICT\\PREDICT 412\\Assignment 2")
diamond=read.csv("two_months_salary.csv", header=TRUE)

library(xtable)
library(psych)
library(pastecs)
library(Hmisc)
library(MMST)
library(car)
library(corrplot)
library(ellipse)
library(PerformanceAnalytics)
library(caret)
library(car)

### CREATE A TRAIN AND TEST SET
set.seed(3456)
trainindex=sample(1:nrow(diamond), 298)
train=diamond[trainindex,]
test=diamond[-trainindex,]
head(train)
head(test)

#####################################
### DESCRIPTIVES and QUALITY CHECK###
#####################################
head(diamond, 5)
dim(diamond)
names(diamond)
#summary(diamond[,-c(4:6)])
#describe(diamond[,-c(4:6)])

xtable(stat.desc(diamond[,-c(4:6)], basic=TRUE, desc=TRUE, norm=TRUE))
#xtabs(~store+channel+cut, data=diamond) 
#xtable(xtabs(~store+channel, data=diamond))

sapply(diamond,class)
sapply(diamond, function(x) sum(is.na(x)))
#xtable(table(diamond$store))
#xtable(table(diamond$channel))
#xtable(table(diamond$channel))

#descriptive table of price by channel and store
#library(doBy)
#xtable(summaryBy(price ~ channel+store, diamond))

##Frequency for store, channel and cut
par(mfrow=c(1,3))
bar charts for categorical variables
ds <- rbind(summary(diamond$store))
ord <- order(ds[1,], decreasing=TRUE)
bp <-  barplot(ds[,ord], beside=TRUE, ylab="Frequency", las=3, ylim=c(0, 250), col=colorspace::rainbow_hcl(1))
text(bp, ds[,ord]+6, ds[,ord])
title(main="Distribution of store")

ds <- rbind(summary(diamond$channel))
ord <- order(ds[1,], decreasing=TRUE)
bp <-  barplot(ds[,ord], beside=TRUE, ylab="Frequency", xlab="channel", ylim=c(0, 400), col=colorspace::rainbow_hcl(1))
text(bp, ds[,ord]+9, ds[,ord])
title(main="Distribution of channel")

ds <- rbind(summary(diamond$cut))
ord <- order(ds[1,], decreasing=TRUE)
bp <-  barplot(ds[,ord], beside=TRUE, ylab="Frequency", xlab="cut", ylim=c(0, 400), col=colorspace::rainbow_hcl(1))
text(bp, ds[,ord]+8, ds[,ord])
title(main="Distribution of cut")

#histogram and QQplots for continuous variables
par(mfrow=c(2,2), cex=0.6)

hist(carat, main="Carat")
with(diamond, {
  hist(carat, breaks="FD", freq=FALSE, ylab="Density")
  lines(density(carat), lwd=2)
  lines(density(carat, adjust=0.5), lwd=1)
rug(carat)
box()
})
#plot20<-Boxplot(diamond$carat, id.n=5, notch=TRUE, ylab="Carat", cex.axis=0.85, col=c("turquoise3"))
qqPlot(diamond$carat, labels=row.names(diamond), id.n=3)

hist(color, main="Color")
with(diamond, {
  hist(color, breaks="FD", freq=FALSE, ylab="Density")
  lines(density(color), lwd=2)
  lines(density(color, adjust=0.5), lwd=1)
rug(color)
box()
})
#plot21<-Boxplot(diamond$color, id.n=5, notch=TRUE, ylab="Color", cex.axis=0.85, col=c("turquoise3"))
qqPlot(diamond$color, labels=row.names(diamond), id.n=3)

hist(clarity, main="Clarity")
with(diamond, {
  hist(clarity, breaks="FD", freq=FALSE, ylab="Density")
  lines(density(clarity), lwd=2)
  lines(density(clarity, adjust=0.5), lwd=1)
rug(clarity)
box()
})
#plot21<-Boxplot(diamond$clarity, id.n=5, notch=TRUE, ylab="Clarity", cex.axis=0.85, col=c("turquoise3"))
qqPlot(diamond$clarity, labels=row.names(diamond), id.n=3)

hist(price, main="Price")
with(diamond, {
  hist(price, breaks="FD", freq=FALSE, ylab="Density")
  lines(density(price), lwd=2)
  lines(density(price, adjust=0.5), lwd=1)
rug(price)
box()
})
#plot21<-Boxplot(diamond$price, id.n=5, notch=TRUE, ylab="Price", cex.axis=0.85, col=c("turquoise3"))
qqPlot(diamond$price, labels=row.names(diamond), id.n=3)





############
### EDA ####
############

#correlation
diamond.matrix<-data.matrix(diamond, rownames.force = NA)
cor(diamond.matrix)
cor_diamond=cor(diamond.matrix, use="complete.obs")
xtable(corstarsl(cor_diamond))

##SCATTERPLOTS AND OUTLIERS
library(car)
scatterplotMatrix(diamond[-c(4:6)], id.n=4)#### PLOT THIS ONE INSTEAD OF CORRELATIONS
#approach 3 -- correlation function

### EDA TREE
library(tree)
tree.data=tree(diamond$price~., diamond)
summary(tree.data)
plot(tree.data)
text(tree.data, pretty=0, cex=0.8)

################
### MODELING ###
################



## 1 naive regression model with backwards variable selection
library(MASS)
model1 <- lm(train$price~.,data=train)
summary(model1)
cbind(Estimate=coef(model1), confint(model1))
model1back<- step(model1, direction="backward")
summary(model1back)
par(mfrow=c(2,2))
plot(model1back)

mean((train$price-predict(model1,test))^2)

#comment on RSE, R2, F   
##compare AIC models
models= list(model1, model1back)
aic= unlist(lapply(models, AIC))
aic

#outlier test
library(car)
par(mfrow=c(2,2))
outlierTest(model1)
#Influence
influenceIndexPlot(model1, vars=c("Cook", "hat"), id.n=3)
influencePlot(model1, id.n=3) 
#durbin watson for autocorrelation of residuals
library(lmtest)
dwtest(model1)
#variance inflation factor for multicollinearity
vif(model1back)



##boxcox power transformation response
bc=boxcox(model1)
which.max(bc$y)#largest log likelihood for y
lambda=bc$x[which.max(bc$y)]## value of lambda
lambda

#transform price
#library(car)
#pricetrain <- yjPower(train$price, lambda)
#pricetest <- yjPower(test$price, lambda)

#transform store
library(plyr)
store2 <- revalue(diamond$store, c("Blue Nile"="Blue Nile", "Ashford"="Ashford", "Ausmans"="Other", "Chalmers"="Other", 
                                   "Danford"="Other", "Fred Meyer"="Other", "Goodmans"="Other", "Kay"="Other", 
                                   "R. Holland"="Other", "Riddles"="Other", "University"="Other", "Zales"="Other"))
storetrain<- revalue(train$store, c("Blue Nile"="Blue Nile", "Ashford"="Ashford", "Ausmans"="Other", "Chalmers"="Other", 
                                   "Danford"="Other", "Fred Meyer"="Other", "Goodmans"="Other", "Kay"="Other", 
                                   "R. Holland"="Other", "Riddles"="Other", "University"="Other", "Zales"="Other"))
storetest<- revalue(test$store, c("Blue Nile"="Blue Nile", "Ashford"="Ashford", "Ausmans"="Other", "Chalmers"="Other", 
                                   "Danford"="Other", "Fred Meyer"="Other", "Goodmans"="Other", "Kay"="Other", 
                                   "R. Holland"="Other", "Riddles"="Other", "University"="Other", "Zales"="Other"))

##bestregsubsets with leaps
library(leaps)
bestreg=regsubsets(price~., data=train, nvmax=15) # xxx depends on the number of variables available. include all
test.mat=model.matrix(price~., data=test)
val.errors=rep(NA,8) #xxx is the number of variables
for (i in 1:8) {
  coefi=coef(bestreg, id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((test$price-pred)^2)
}##model with 8 is best
bestreg=regsubsets(price~., data=train, nvmax=)
coef(bestregfull, )



#use to remove observations
#modelbis=update(model, subset=-c(xxx,xxx))



#################
## CORRELATION ##
#################
#approach1
#diamond.matrix<-data.matrix(diamond, rownames.force = NA)
#cor(diamond.matrix)
#cor_diamond=cor(diamond.matrix, use="complete.obs")
#colramp = colorRampPalette(c("white", blues9))
#colors = colramp(100)
#my.plotcorr(cor_diamond, col=colors[((cor_diamond + 1)/2) * 100], diag='ellipse', 
#            upper.panel="number", mar=c(0,2,0,0))
#approach2
#library(PerformanceAnalytics)
#chart.Correlation(cor_diamond)





