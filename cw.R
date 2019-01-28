
# course work

#--------- Q1 ------

data1 <- read.table("pigeon.txt", sep = "\t", dec = ".", header = TRUE)
attach(data1)

##-------- a) ------

# The appropriate selected default distribution for response varibale
# is poisson distribution because it is count data.
hist(data1$PIGU)
plot(table(PIGU), type = "h")
plot(ObservCond, PIGU)
plot(Bay, PIGU)
plot(Year, PIGU)

# from the histogram plot, it shows that the response variable is zero inflated
# hence the appropriate model to used is zero-inflated models
length(which(data1$PIGU == 0)) # shows the numbers of zero observation

library(pscl)

# this model is using poisson distribution with all explanatory variables
model.1<-zeroinfl(PIGU ~ Temp  + Year + Bay + ObservCond | 1 , data=data1, dist="poisson")
summary(model.1)

# this model is using poisson distribution without explanatory variable Year
model.2<-zeroinfl(PIGU ~ Temp + Bay  + ObservCond | 1 , data=data1, dist="poisson")
summary(model.2)

# this model is using poisson distribution without explanatory variable Year
# and the prob of 0 is model with explanatory variable ObservCond
model.3<-zeroinfl(PIGU ~ Temp + Bay + ObservCond | ObservCond , data=data1, dist="poisson")
summary(model.3)

# this model is using poisson distribution without explanatory variable Year
# and the prob of 0 is model with explanatory variables ObservCond and Bay
model.4<-zeroinfl(PIGU ~ Temp + Bay + ObservCond |ObservCond + Bay , data=data1, dist="poisson")
summary(model.4)

# this model is using poisson distribution without explanatory variable Year
# and the prob of 0 is model with explanatory variables ObservCond Year and Bay
model.5<-zeroinfl(PIGU ~ Temp + Bay + ObservCond |Year + ObservCond + Bay , data=data1, dist="poisson")
summary(model.5)

# using AIC to compare the models because they are from the same distribution
AIC(model.1)
AIC(model.2)
AIC(model.3)
AIC(model.4)
AIC(model.5)
# base in AIC, the best from these five is model.5, AIC = 9333.505

# Here I am trying the 5 models above with different distribution which is negative binomial

# this model is using negbin distribution with all explanatory variables
model.6<-zeroinfl(PIGU ~ Temp  + Year + Bay + ObservCond | 1 , data=data1, dist="negbin")
summary(model.6)

# this model is using negative binomial distribution without explanatory variable Year
model.7<-zeroinfl(PIGU ~ Temp + Bay + ObservCond | 1 , data=data1, dist="negbin")
summary(model.7)

# this model is using negative binomial distribution without explanatory variable Year
# and the prob of 0 is model with explanatory variable ObservCond
model.8<-zeroinfl(PIGU ~ Temp + Bay + ObservCond | ObservCond , data=data1, dist="negbin")
summary(model.8)

# this model is using negative binomial distribution without explanatory variable Year
# and the prob of 0 is model with explanatory variables ObservCond and Bay
model.9<-zeroinfl(PIGU ~ Temp + Bay + ObservCond | ObservCond + Bay , data=data1, dist="negbin")
summary(model.9)

# this model is using negative binomial distribution without explanatory variable Year
# and the prob of 0 is model with explanatory variable Bay
model.10<-zeroinfl(PIGU ~ Temp + Bay + ObservCond |Bay , data=data1, dist="negbin")
summary(model.10)

# using AIC to compare the next 5 models because they are from the same distribution
AIC(model.6)
AIC(model.7)
AIC(model.8)
AIC(model.9)
AIC(model.10)
# base on AIC the best model from these five is model.6, AIC is 9014.603

# Here I am comparing all models using MSE
library(MASS)
MSE.M1<-sum((PIGU-fitted(model.1, type="response"))^2)/3793
MSE.M2<-sum((PIGU-fitted(model.2, type="response"))^2)/3793
MSE.M3<-sum((PIGU-fitted(model.3, type="response"))^2)/3793
MSE.M4<-sum((PIGU-fitted(model.4, type="response"))^2)/3793
MSE.M5<-sum((PIGU-fitted(model.5, type="response"))^2)/3793
MSE.M6<-sum((PIGU-fitted(model.6, type="response"))^2)/3793
MSE.M7<-sum((PIGU-fitted(model.7, type="response"))^2)/3793

MSE.M1
MSE.M2
MSE.M3
MSE.M4
MSE.M5
MSE.M6
MSE.M7

### Tentatively, chosen models are model 5 and model 6
# model.5
# model.6

### Here I performed Hypothesis testing of the explanatory variables with the model 5
library(lmtest)

model.5<-zeroinfl(PIGU ~ Temp + Bay + ObservCond |Year + ObservCond + Bay , data=data1, dist="poisson")
summary(model.5)
 
# without Year in determination of prob. of zero
model.H0 <- zeroinfl(PIGU ~ Temp + Bay + ObservCond | ObservCond + Bay , data=data1, dist="poisson")
waldtest(model.H0, model.5)
# The result of the test shows that Year is not statistically significant

# withou ObservCond in determination of prob. of zero
model.H0 <- zeroinfl(PIGU ~ Temp + Bay + ObservCond | Year + Bay , data=data1, dist="poisson")
waldtest(model.H0, model.5)
# The result of the test shows that ObservCond is statistically significant

# without Bay in determination of prob. of zero
model.H0 <- zeroinfl(PIGU ~ Temp + Bay + ObservCond | ObservCond + Year , data=data1, dist="poisson")
waldtest(model.H0, model.5)
# The result of the test shows that Bay is statistically significant

## finalized model from poisson distribution:
model.poi <- zeroinfl(PIGU ~ Temp + Bay + ObservCond | ObservCond + Bay , data=data1, dist="poisson")
summary(model.poi)


### Here I performed Hypothesis testing of the explanatory variables with the model 6
# testing statistical significant of Year
model.6<-zeroinfl(PIGU ~ Temp + Year  + Bay + ObservCond | 1 , data=data1, dist="negbin")
summary(model.6)

model.H0 <- zeroinfl(PIGU ~ Temp  + Bay + ObservCond | 1 , data=data1, dist="negbin")
waldtest(model.H0, model.6)
# The result of the test shows that Year is not statistically significant

# testing statistical significant of Bay
model.6<-zeroinfl(PIGU ~ Temp + Year  + Bay + ObservCond | 1 , data=data1, dist="negbin")
summary(model.6)

model.H0 <- zeroinfl(PIGU ~ Year + Temp + ObservCond | 1 , data=data1, dist="negbin")
waldtest(model.H0, model.6)
# The result of the test shows that Bay is statistically significant

## finalized model from negbin distribution:
model.ngb <- zeroinfl(PIGU ~  Temp + Bay + ObservCond | 1 , data=data1, dist="negbin")
summary(model.ngb)

MSE.poi<-sum((PIGU-fitted(model.poi, type="response"))^2)/3793
MSE.ngb<-sum((PIGU-fitted(model.ngb, type="response"))^2)/3793

# final model is model.poi because its MSE.poi is a bit lower than model.ngb
## ------- b) -------
model.poi <- zeroinfl(PIGU ~ Temp + Bay + ObservCond | ObservCond + Bay , data=data1, dist="poisson")
summary(model.poi)

newdata <- data.frame(Temp = 4.5, Bay = "W. Sitkalidak", ObservCond = "ExcellentIdeal")
mu.pred <- predict(model.poi, newdata = newdata, type = "response")

# prediction interval for the new observation
# using my model is mixed effect model, so I used quantile for prediction interval
lowerY <- qpois(0.05, lambda = mu.pred)
upperY <- qpois(0.95, lambda = mu.pred)

## ------ c) --------
# testing 5% significant level of explanatory variable ObservCond

model.H0 <-  zeroinfl(PIGU ~ Temp + Bay | Bay , data=data1, dist="poisson")
waldtest(model.H0, model.poi)
waldtest(model.H0, model.poi)$Chisq[2]
# explanatory variable ObservCond is statisticaly significant

#--------- Q2 ------

## a)
data2 <- read.table("caffeine.txt", sep = "\t", dec = ".", header = TRUE)
attach(data2)

hist(data2$Caffeine)
# The appropriate selected default distribution for response varibale
# is Normal distribution because the histogram plot of the response variable looks normal
min(data2$Caffeine)
max(data2$Caffeine)

plot(Brand, Caffeine)
plot(Formulation, Caffeine)

interaction.plot(Formulation, Restaurant, Caffeine)
interaction.plot(Formulation, Brand, Caffeine)

# Model using Normal distribution with identity link
model.21 <- glm(Caffeine ~ Brand + Formulation + Restaurant, data = data2, family = gaussian(link = "identity"))
summary(model.21)

model.22 <- glm(Caffeine ~ Brand + Formulation + Restaurant, data = data2, family = gaussian(link = "log"))
summary(model.22)

model.23 <- glm(Caffeine ~ Brand + Formulation + Restaurant, data = data2, family = gaussian(link = "inverse"))
summary(model.23)

AIC(model.21)
AIC(model.22)
AIC(model.23)
# lowest is model.23, the model with inverse link
interaction.plot(Formulation, Brand, fitted(model.23, type="response"))
interaction.plot(Formulation, Restaurant, fitted(model.23, type="response"))

# Here I try to repeat the above model with Gamma distribution 
# and Inverse Gamma distibution, to see I get come up with better model.

# Repeat Models above using Gamma distribution
model.24 <- glm(Caffeine ~ Brand + Formulation + Restaurant, data = data2, family = Gamma(link = "identity"))
summary(model.24)

model.25 <- glm(Caffeine ~ Brand + Formulation, data = data2, family = Gamma(link = "log"))
summary(model.25)

model.26 <- glm(Caffeine ~ Brand + Formulation + Restaurant, data = data2, family = Gamma(link = "inverse"))
summary(model.26)

AIC(model.24)
AIC(model.25)
AIC(model.26)
# lowest is model.26, the model with inverse link
interaction.plot(Formulation, Brand, fitted(model.26, type="response"))
interaction.plot(Formulation, Restaurant, fitted(model.26, type="response"))

# Repeat Models above using Inverse Gaussian distribution
model.27 <- glm(Caffeine ~ Brand + Formulation + Restaurant, data = data2, family = inverse.gaussian(link = "identity"))
summary(model.27)

model.28 <- glm(Caffeine ~ Brand + Formulation, data = data2, family = inverse.gaussian(link = "log"))
summary(model.28)

model.29 <- glm(Caffeine ~ Brand + Formulation + Restaurant, data = data2, family = inverse.gaussian(link = "inverse"))
summary(model.29)

AIC(model.27)
AIC(model.28)
AIC(model.29)
# lowest is model.29, model with inverse link
interaction.plot(Formulation, Brand, fitted(model.29, type="response"))
interaction.plot(Formulation, Restaurant, fitted(model.29, type="response"))

# Here I compared the best model from each distribution

shapiro.test(residuals(model.23, type="pearson"))
shapiro.test(residuals(model.26, type="pearson"))
shapiro.test(residuals(model.29, type="pearson"))

# Both Gamma and Inverse gaussian distribution could be choosen as suitable 
# distribution for the model. The normal distribution model is having lowest p-value
# and it is good to consider model with higher p-value.

# Tentatively, model with Gamma distribution with inverse link, model.26 is choosen.
# Checking the fitness of the model
model.26 <- glm(Caffeine ~ Brand + Formulation + Restaurant, data = data2, family = Gamma(link = "inverse"))
summary(model.26)

# Plotting
interaction.plot(Formulation, Brand, Caffeine)
interaction.plot(Formulation, Restaurant, Caffeine)

interaction.plot(Formulation,Brand,fitted(model.26, type="response"))
interaction.plot(Formulation,Restaurant,fitted(model.26, type="response"))

# Here I checked for statistical significant of Restaurant
model.H1 <- model.26
model.H0 <- glm(Caffeine ~ Brand + Formulation, data = data2, family = Gamma(link = "inverse"))

anova(model.H0, model.H1, test = "F")
anova(model.H0, model.H1, test = "F")$F[2]

# I removed restaurant from the model and examine the model with different links 
model.inverse <- glm(Caffeine ~ Brand + Formulation, data = data2, family = Gamma(link = "inverse"))
summary(model.inverse)

model.log <- glm(Caffeine ~ Brand + Formulation, data = data2, family = Gamma(link = "log"))
summary(model.log)

model.identity <- glm(Caffeine ~ Brand + Formulation, data = data2, family = Gamma(link = "identity"))
summary(model.identity)

AIC(model.inverse)
AIC(model.log) # finaling choose gamma dist with log link
AIC(model.identity)
interaction.plot(Formulation,Brand,fitted(model.log, type="response"))
## b)
# predictive effect size difference
coef(model.log)
# y1f.star
x1f <- t(cbind(1,0,0))

newdata <- data.frame(Brand = "Coke", Formulation = "Diet")
pred<-predict(model.log, newdata=newdata, type="response") 

eta.hat<-predict(model.log, newdata=newdata, type="link")
cov.eta<-as.numeric(t(x1f)%*%vcov(model.log)%*%(x1f))
eta.star<-rnorm(1000, mean=eta.hat, sd=sqrt(cov.eta))
mu.star<-exp(eta.star)
var.star<-summary(model.log)$dispersion*mu.star^2
a.star<-(mu.star^2)/var.star
s.star<-var.star/mu.star

y1f.star<-rgamma(1000, shape=a.star,scale=s.star)


# y2f.star
x2f <- t(cbind(1,1,0))

newdata <- data.frame(Brand = "Pepsi", Formulation = "Diet")
pred<-predict(model.log, newdata=newdata, type="response") 

eta.hat<-predict(model.log, newdata=newdata, type="link")
cov.eta<-as.numeric(t(x2f)%*%vcov(model.log)%*%(x2f))
eta.star<-rnorm(1000, mean=eta.hat, sd=sqrt(cov.eta))
mu.star<-exp(eta.star)
var.star<-summary(model.log)$dispersion*mu.star^2
a.star<-(mu.star^2)/var.star
s.star<-var.star/mu.star

y2f.star<-rgamma(1000, shape=a.star,scale=s.star)
hist(y2f.star)

# 80% prediction interval for predictive effect size
diff.star <- y2f.star-y1f.star
lower.bound<-quantile(diff.star, c(0.1))
upper.bound<-quantile(diff.star, 1-c(0.1))
lower.bound
upper.bound


## c)

# Testing if the Brand is statistically significant
model.H1 <- model.log
model.H0 <- glm(Caffeine ~ Formulation, data = data2, family = Gamma(link = "log"))

anova(model.H0, model.H1, test = "F")
anova(model.H0, model.H1, test = "F")$F[2]
# Brand is statistically significant
