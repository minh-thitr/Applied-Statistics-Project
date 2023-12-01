
#Read file
library("readxl")
library(lattice)

wkbk <- read_excel("/Users/thitruong/Desktop/Illinois Tech/Fall 2022/MATH 484/Project/CensusIncome.xlsx")

X1 <- as.factor(wkbk$Age) #age
X2 <- as.factor(wkbk$Gender) #gender
X3 <- as.factor(wkbk$Education) #education
X4 <- as.factor(wkbk$Race) #race
X5 <- as.numeric(wkbk$n) 
X6 <- as.numeric(wkbk$StandardError) #standard error
Y <- as.numeric(wkbk$MeanEarning) #mean earning

##SLR stuff
dotplot(X2~Y, data=wkbk,xlab="Mean Earning",ylab="Gender",main="Mean Earning vs. Gender")
dotplot(X1~Y, data=wkbk,xlab="Mean Earning",ylab="Age",main="Mean Earning vs. Age")
dotplot(X3~Y, data=wkbk,xlab="Mean Earning",ylab="Education",main="Mean Earning vs. Education")
dotplot(X4~Y, data=wkbk,xlab="Mean Earning",ylab="Race",main="Mean Earning vs. Race")

#is race and education independent? [NO]
#codependent IF not equally dist across education (by race)
RE = aggregate(X5~X4+X3,data=wkbk,sum)
  #Race(total): HS,Associates,BS+
   #A(6337):  .12,.06,.82
   #B(9995):  .37,.15,.49
   #H(11605): .48,.15,.38
   #W(66206): .30,.14,.56
pay_race = aggregate(Y~X4,data=wkbk,mean)

#is gender and education independent? [ALMOST]
GE = aggregate(X5~X2+X3,data=wkbk,sum)
  #Gender(total): HS,Associates,BS+
   #F(41465):   .26,.14,.60
   #M(52678):   .37,.13,.50
pay_gender = aggregate(Y~X2,data=wkbk,mean)

pay_edu = aggregate(Y~X3,data=wkbk,mean)
pay_age = aggregate(Y~X1,data=wkbk,mean)

#simple linear regression for each variables
fit_age <- lm(Y~X1,data=wkbk)
summary(fit_age)
plot(fit_age)
anova(fit_age)


fit_gender <- lm(Y~X2,data=wkbk)
summary(fit_gender)
plot(fit_gender)
anova(fit_gender)


fit_edu <- lm(Y~X3,data=wkbk)
summary(fit_edu)
plot(fit_edu)
anova(fit_edu)


fit_race <- lm(Y~X4,data=wkbk)
summary(fit_race)
plot(fit_race)
anova(fit_race)


#MLR with the standard error
fit <- lm(Y ~ X1+X2+X3+X4,data=wkbk)
summary(fit)
plot(fit)

#residuals vs fitted plot
plot(fit$fitted.values, fit$residuals, xlab="Fitted Value",ylab = "Residuals")

#considering standard error as weighted
w <- 1 / X6^2

#Weighted least squares regression
fit.weighted <- lm(Y ~ X1+X2+X3+X4,data=wkbk,weights = w)
summary(fit.weighted)
plot(fit.weighted)

anova(fit.weighted)

#pairs(Y~X1+X2+X3+X4,data=wkbk)



  