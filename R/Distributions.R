###
### Statistical Analysis and Reporting in R
### Jacob O. Wobbrock, Ph.D.
### wobbrock@uw.edu
### The Information School
### University of Washington
### August 2, 2023
### Updated: 3/12/2025
###

###
### Distributions.R
### (Normal, lognormal, Poisson, negative binomial, exponential, gamma)
###

library(plyr) # for ddply
library(EnvStats) # for gofTest
library(fitdistrplus) # for fitdist, gofstat
library(performance) # for check_overdispersion


##
#### 1. Normal distribution ####
##
# df has one factor (X) w/two levels (a,b) and continuous response Y
set.seed(123)
a = round(rnorm(30, mean=30.0, sd=10.0), digits=2)
b = round(rnorm(30, mean=45.0, sd=15.0), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b"), each=30)),
  Y = c(a,b)
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))

fa = gofTest(df[df$X == "a",]$Y, distribution="norm")
fb = gofTest(df[df$X == "b",]$Y, distribution="norm")

par(mfrow=c(2,1))
  hist(df[df$X == "a",]$Y, 
       main="Histogram of Y for X=a", 
       xlab="Y",
       col="pink", 
       xlim=c(10, 80), 
       ylim=c(0, 0.05), 
       breaks=seq(10, 80, 10),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(10, 80, 10), labels=seq(10, 80, 10))
  axis(side=2, at=seq(0, 0.05, 0.01), labels=seq(0, 0.05, 0.01))
  curve(dnorm(x, mean=fa$distribution.parameters[1], sd=fa$distribution.parameters[2]), lty=1, lwd=3, col="red", add=TRUE)
  
  hist(df[df$X == "b",]$Y, 
       main="Histogram of Y for X=b", 
       xlab="Y",
       col="lightblue", 
       xlim=c(10, 80), 
       ylim=c(0, 0.05), 
       breaks=seq(10, 80, 10),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(10, 80, 10), labels=seq(10, 80, 10))
  axis(side=2, at=seq(0, 0.05, 0.01), labels=seq(0, 0.05, 0.01))
  curve(dnorm(x, mean=fb$distribution.parameters[1], sd=fb$distribution.parameters[2]), lty=1, lwd=3, col="blue", add=TRUE)
par(mfrow=c(1,1))

print(fa)
print(fb)



##
#### 2. Lognormal distribution ####
##
# df has one factor (X) w/two levels (a,b) and positivey skewed response Y
set.seed(123)
a = round(rlnorm(30, meanlog=2.50, sdlog=0.90), digits=2)
b = round(rlnorm(30, meanlog=2.65, sdlog=1.00), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b"), each=30)),
  Y = c(a,b)
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))

fa = gofTest(df[df$X == "a",]$Y, distribution="lnorm")
fb = gofTest(df[df$X == "b",]$Y, distribution="lnorm")

par(mfrow=c(2,1))
  hist(df[df$X == "a",]$Y, 
       main="Histogram of Y for X=a", 
       xlab="Y",
       col="pink", 
       xlim=c(0,140), 
       ylim=c(0, 0.06), 
       breaks=seq(0, 140, 20),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 140, 20), labels=seq(0, 140, 20))
  axis(side=2, at=seq(0, 0.06, 0.01), labels=seq(0, 0.06, 0.01))
  curve(dlnorm(x, meanlog=fa$distribution.parameters[1], sdlog=fa$distribution.parameters[2]), lty=1, lwd=3, col="red", add=TRUE)
  
  hist(df[df$X == "b",]$Y, 
       main="Histogram of Y for X=b", 
       xlab="Y",
       col="lightblue", 
       xlim=c(0,140), 
       ylim=c(0, 0.06), 
       breaks=seq(0, 140, 20),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 140, 20), labels=seq(0, 140, 20))
  axis(side=2, at=seq(0, 0.06, 0.01), labels=seq(0, 0.06, 0.01))
  curve(dlnorm(x, meanlog=fb$distribution.parameters[1], sdlog=fb$distribution.parameters[2]), lty=1, lwd=3, col="blue", add=TRUE)
par(mfrow=c(1,1))

print(fa)
print(fb)



##
#### 3. Poisson distribution ####
##
# df has one factor (X) w/two levels (a,b) and integer count response Y
set.seed(123)
a = round(rpois(30, lambda=4.00), digits=2)
b = round(rpois(30, lambda=6.00), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b"), each=30)),
  Y = c(a,b)
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))

fa = fitdist(df[df$X == "a",]$Y, distr="pois")
fb = fitdist(df[df$X == "b",]$Y, distr="pois")

par(mfrow=c(2,1))
  hist(df[df$X == "a",]$Y, 
       main="Histogram of Y for X=a", 
       xlab="Y",
       col="pink", 
       xlim=c(0,12), 
       ylim=c(0, 0.20), 
       breaks=seq(0, 12, 2),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 12, 2), labels=seq(0, 12, 2))
  axis(side=2, at=seq(0, 0.20, 0.05), labels=seq(0, 0.20, 0.05))
  curve(dpois(round(x,0), lambda=fa$estimate[1]), lty=1, lwd=3, col="red", add=TRUE)
  xa = seq(floor(min(df[df$X == "a",]$Y)), ceiling(max(df[df$X == "a",]$Y)), by=1)
  lines(xa, dpois(xa, lambda=fa$estimate[1]), lty=1, lwd=3, col="darkred")
  
  hist(df[df$X == "b",]$Y, 
       main="Histogram of Y for X=b", 
       xlab="Y",
       col="lightblue", 
       xlim=c(0,12), 
       ylim=c(0, 0.20), 
       breaks=seq(0, 12, 2),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 12, 2), labels=seq(0, 12, 2))
  axis(side=2, at=seq(0, 0.20, 0.05), labels=seq(0, 0.20, 0.05))
  curve(dpois(round(x,0), lambda=fb$estimate[1]), lty=1, lwd=3, col="blue", add=TRUE)
  xb = seq(floor(min(df$Y)), ceiling(max(df$Y)), by=1)
  lines(xb, dpois(xb, lambda=fb$estimate[1]), lty=1, lwd=3, col="darkblue")
par(mfrow=c(1,1))

gofstat(fa)
gofstat(fb)

# if var/|mean| > 1.15, we have overdispersion; use quasipoisson or an nbinom GL(M)M
var(df[df$X == "a",]$Y) / abs(mean(df[df$X == "a",]$Y)) > 1.15
var(df[df$X == "b",]$Y) / abs(mean(df[df$X == "b",]$Y)) > 1.15

# another way to check for overdispersion
m = glm(Y ~ X, data=df, family=poisson)
print(check_overdispersion(m))



##
#### 4. Negative binomial distribution ####
##
# df has one factor (X) w/two levels (a,b) and integer count response Y
set.seed(123)
a = round(rnbinom(30, size=4.0, mu=6.0), digits=2)
b = round(rnbinom(30, size=6.0, mu=4.0), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b"), each=30)),
  Y = c(a,b)
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))

fa = fitdist(df[df$X == "a",]$Y, distr="nbinom")
fb = fitdist(df[df$X == "b",]$Y, distr="nbinom")

par(mfrow=c(2,1))
  hist(df[df$X == "a",]$Y, 
       main="Histogram of Y for X=a", 
       xlab="Y",
       col="pink", 
       xlim=c(0,16), 
       ylim=c(0, 0.25), 
       breaks=seq(0, 16, 2),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 16, 2), labels=seq(0, 16, 2))
  axis(side=2, at=seq(0, 0.25, 0.05), labels=seq(0, 0.25, 0.05))
  curve(dnbinom(round(x,0), size=fa$estimate[1], mu=fa$estimate[2]), lty=1, lwd=3, col="red", add=TRUE)
  xa = seq(floor(min(df[df$X == "a",]$Y)), ceiling(max(df[df$X == "a",]$Y)), by=1)
  lines(xa, dnbinom(xa, size=fa$estimate[1], mu=fa$estimate[2]), lty=1, lwd=3, col="darkred")
  
  hist(df[df$X == "b",]$Y, 
       main="Histogram of Y for X=b", 
       xlab="Y",
       col="lightblue", 
       xlim=c(0,16), 
       ylim=c(0, 0.25), 
       breaks=seq(0, 16, 2),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 16, 2), labels=seq(0, 16, 2))
  axis(side=2, at=seq(0, 0.25, 0.05), labels=seq(0, 0.25, 0.05))
  curve(dnbinom(round(x,0), size=fb$estimate[1], mu=fb$estimate[2]), lty=1, lwd=3, col="blue", add=TRUE)
  xb = seq(floor(min(df[df$X == "b",]$Y)), ceiling(max(df[df$X == "b",]$Y)), by=1)
  lines(xb, dnbinom(xb, size=fb$estimate[1], mu=fb$estimate[2]), lty=1, lwd=3, col="darkblue")
par(mfrow=c(1,1))

gofstat(fa)
gofstat(fb)

# if var/|mean| > 1.15, we have overdispersion; use quasipoisson or a nbinom GLM
var(df[df$X == "a",]$Y) / abs(mean(df[df$X == "a",]$Y)) > 1.15
var(df[df$X == "b",]$Y) / abs(mean(df[df$X == "b",]$Y)) > 1.15

# another way to check for overdispersion
m = glm(Y ~ X, data=df, family=poisson)
print(check_overdispersion(m))



##
#### 5. Exponential distribution ####
##
# df has one factor (X) w/two levels (a,b) and exponential response Y
set.seed(123)
a = round(rexp(30, rate=1/10), digits=2)
b = round(rexp(30, rate=1/12), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b"), each=30)),
  Y = c(a,b)
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))

fa = gofTest(df[df$X == "a",]$Y, distribution="exp")
fb = gofTest(df[df$X == "b",]$Y, distribution="exp")

par(mfrow=c(2,1))
  hist(df[df$X == "a",]$Y, 
       main="Histogram of Y for X=a", 
       xlab="Y",
       col="pink", 
       xlim=c(0, 90), 
       ylim=c(0, 0.1), 
       breaks=seq(0, 90, 10),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 90, 10), labels=seq(0, 90, 10))
  axis(side=2, at=seq(0, 0.1, 0.025), labels=seq(0, 0.1, 0.025))
  curve(dexp(x, rate=fa$distribution.parameters[1]), lty=1, lwd=3, col="red", add=TRUE)
  
  hist(df[df$X == "b",]$Y, 
       main="Histogram of Y for X=b", 
       xlab="Y",
       col="lightblue", 
       xlim=c(0, 90), 
       ylim=c(0, 0.1), 
       breaks=seq(0, 90, 10),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 90, 10), labels=seq(0, 90, 10))
  axis(side=2, at=seq(0, 0.1, 0.025), labels=seq(0, 0.1, 0.025))
  curve(dexp(x, rate=fb$distribution.parameters[1]), lty=1, lwd=3, col="blue", add=TRUE)
par(mfrow=c(1,1))

print(fa)
print(fb)



##
#### 6. Gamma distribution ####
##
# df has one factor (X) w/two levels (a,b) and positively skewed response Y
set.seed(123)
a = round(rgamma(30, shape=5.0, scale=4.0), digits=2)
b = round(rgamma(30, shape=3.0, scale=3.0), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b"), each=30)),
  Y = c(a,b)
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))

fa = gofTest(df[df$X == "a",]$Y, distribution="gamma")
fb = gofTest(df[df$X == "b",]$Y, distribution="gamma")

par(mfrow=c(2,1))
  hist(df[df$X == "a",]$Y, 
       main="Histogram of Y for X=a", 
       xlab="Y",
       col="pink", 
       xlim=c(0, 40), 
       ylim=c(0, 0.12), 
       breaks=seq(0, 40, 5),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 40, 5), labels=seq(0, 40, 5))
  axis(side=2, at=seq(0, 0.12, 0.02), labels=seq(0, 0.12, 0.02))
  curve(dgamma(x, shape=fa$distribution.parameters[1], scale=fa$distribution.parameters[2]), lty=1, lwd=3, col="red", add=TRUE)
  
  hist(df[df$X == "b",]$Y, 
       main="Histogram of Y for X=b", 
       xlab="Y",
       col="lightblue", 
       xlim=c(0, 40), 
       ylim=c(0, 0.12), 
       breaks=seq(0, 40, 5),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 40, 5), labels=seq(0, 40, 5))
  axis(side=2, at=seq(0, 0.12, 0.02), labels=seq(0, 0.12, 0.02))
  curve(dgamma(x, shape=fa$distribution.parameters[1], scale=fa$distribution.parameters[2]), lty=1, lwd=3, col="blue", add=TRUE)
par(mfrow=c(1,1))

print(fa)
print(fb)

