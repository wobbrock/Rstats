###
### Statistical Analysis and Reporting in R
### Jacob O. Wobbrock, Ph.D.
### wobbrock@uw.edu
### The Information School
### University of Washington
### March 12, 2019
### Updated: 11/11/2024
###

###
### GLM.R
### (Generalized linear models)
###

library(plyr) # for ddply
library(car) # for Anova
library(emmeans) # for emmeans
library(nnet) # for multinom
library(multpois) # for glm.mp, Anova.mp, glm.mp.con
library(MASS) # for polr
library(performance) # for check_*
library(glmmTMB) # for glmmTMB
library(MASS) # for glm.nb


###
#### One Between-Ss. Factor (3 levels) ####
###

##
#### 1. Normal ####
##
# df has one between-Ss. factor (X) w/levels (a,b,c) and continuous response (Y)
set.seed(123)
a = round(rnorm(20, mean=30.0, sd=12.0), digits=2)
b = round(rnorm(20, mean=45.0, sd=15.0), digits=2)
c = round(rnorm(20, mean=40.0, sd=14.0), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
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
boxplot(Y ~ X, data=df, main="Y by X", col=c("pink","lightblue","lightgreen"))

par(mfrow=c(3,1))
  hist(df[df$X == "a",]$Y, main="Y by X=a", xlab="Y", xlim=c(0,80), ylim=c(0,8), breaks=seq(0,80,10), col="pink")
  hist(df[df$X == "b",]$Y, main="Y by X=b", xlab="Y", xlim=c(0,80), ylim=c(0,8), breaks=seq(0,80,10), col="lightblue")
  hist(df[df$X == "c",]$Y, main="Y by X=c", xlab="Y", xlim=c(0,80), ylim=c(0,8), breaks=seq(0,80,10), col="lightgreen")
par(mfrow=c(1,1))

m = lm(Y ~ X, data=df)
print(check_normality(m))
print(check_homogeneity(m))
Anova(m, type=3, test.statistic="F")
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 2. Lognormal ####
##
# df has one between-Ss. factor (X) w/levels (a,b,c) and skewed continuous response (Y)
set.seed(123)
a = round(rlnorm(20, meanlog=2.30, sdlog=0.90), digits=2)
b = round(rlnorm(20, meanlog=2.90, sdlog=1.00), digits=2)
c = round(rlnorm(20, meanlog=1.95, sdlog=1.10), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
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
boxplot(Y ~ X, data=df, main="Y by X", col=c("pink","lightblue","lightgreen"))

par(mfrow=c(3,1))
  hist(df[df$X == "a",]$Y, main="Y by X=a", xlab="Y", xlim=c(0,80), ylim=c(0,14), breaks=seq(0,80,10), col="pink")
  hist(df[df$X == "b",]$Y, main="Y by X=b", xlab="Y", xlim=c(0,80), ylim=c(0,14), breaks=seq(0,80,10), col="lightblue")
  hist(df[df$X == "c",]$Y, main="Y by X=c", xlab="Y", xlim=c(0,80), ylim=c(0,14), breaks=seq(0,80,10), col="lightgreen")
par(mfrow=c(1,1))

m0 = lm(Y ~ X, data=df)
print(check_normality(m0))
print(check_homogeneity(m0))

m = lm(log(Y) ~ X, data=df)
print(check_normality(m))
print(check_homogeneity(m))
Anova(m, type=3, test.statistic="F")
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 3. Binomial ####
##
# df has one between-Ss. factor (X) w/levels (a,b,c) and dichotomous response (Y)
set.seed(123)
a = sample(c("no", "yes"), 20, replace=TRUE, prob=c(0.7, 0.3))
b = sample(c("no", "yes"), 20, replace=TRUE, prob=c(0.3, 0.7))
c = sample(c("no", "yes"), 20, replace=TRUE, prob=c(0.5, 0.5))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = factor(c(a,b,c))
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "no"=sum(data$Y == "no"),
  "yes"=sum(data$Y == "yes")
))
mosaicplot( ~ X + Y, data=df, main="Y by X", col=c("pink","lightgreen"))

m = glm(Y ~ X, data=df, family=binomial)
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 4. Multinomial ####
##
# df has one between-Ss. factor (X) w/levels (a,b,c) and polytomous response (Y)
set.seed(123)
a = sample(c("maybe","no","yes"), 20, replace=TRUE, prob=c(0.5, 0.3, 0.2))
b = sample(c("maybe","no","yes"), 20, replace=TRUE, prob=c(0.3, 0.2, 0.5))
c = sample(c("maybe","no","yes"), 20, replace=TRUE, prob=c(0.4, 0.5, 0.1))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = factor(c(a,b,c))
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "maybe"=sum(data$Y == "maybe"),
  "no"=sum(data$Y == "no"),
  "yes"=sum(data$Y == "yes")
))
mosaicplot( ~ X + Y, data=df, main="Y by X", col=c("lightyellow","pink","lightgreen"))

m0 = multinom(Y ~ X, data=df, trace=FALSE)
Anova(m0, type=3)
# post hoc tests with emmeans are thorny for multinom models

m = glm.mp(Y ~ X, data=df) # use the multinomial-Poisson trick
Anova.mp(m, type=3)
glm.mp.con(m, pairwise ~ X, adjust="holm")



##
#### 5. Ordinal ####
##
# df has one between-Ss. factor (X) w/levels (a,b,c) and ordinal response (1-7)
set.seed(123)
a = sample(1:7, 20, replace=TRUE, prob=c(0.10, 0.10, 0.20, 0.30, 0.40, 0.40, 0.20))
b = sample(1:7, 20, replace=TRUE, prob=c(0.80, 0.70, 0.60, 0.30, 0.40, 0.20, 0.10))
c = sample(1:7, 20, replace=TRUE, prob=c(0.00, 0.20, 0.10, 0.90, 0.80, 0.80, 0.70))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
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
mosaicplot( ~ X + Y, data=df, main="Y by X", col=terrain.colors(7))

boxplot(Y ~ X, main="Y by X", data=df, col=c("pink","lightblue","lightgreen"))

par(mfrow=c(3,1))
  hist(df[df$X == "a",]$Y, main="Y by X=a", xlab="Y", xlim=c(1,7), ylim=c(0,8), breaks=seq(1,7,1), col="pink")
  hist(df[df$X == "b",]$Y, main="Y by X=b", xlab="Y", xlim=c(1,7), ylim=c(0,8), breaks=seq(1,7,1), col="lightblue")
  hist(df[df$X == "c",]$Y, main="Y by X=c", xlab="Y", xlim=c(1,7), ylim=c(0,8), breaks=seq(1,7,1), col="lightgreen")
par(mfrow=c(1,1))

df$Y = ordered(df$Y)
m = polr(Y ~ X, data=df, Hess=TRUE)
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 6. Poisson ####
##
# df has one between-Ss. factor (X) w/levels (a,b,c) and count response (Y)
set.seed(123)
a = round(rpois(20, lambda=4.00), digits=0)
b = round(rpois(20, lambda=6.00), digits=0)
c = round(rpois(20, lambda=5.00), digits=0)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
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
boxplot(Y ~ X, data=df, main="Y by X", col=c("pink","lightblue","lightgreen"))

par(mfrow=c(3,1))
  hist(df[df$X == "a",]$Y, main="Y by X=a", xlab="Y", xlim=c(0,14), ylim=c(0,11), breaks=seq(0,14,2), col="pink")
  hist(df[df$X == "b",]$Y, main="Y by X=b", xlab="Y", xlim=c(0,14), ylim=c(0,11), breaks=seq(0,14,2), col="lightblue")
  hist(df[df$X == "c",]$Y, main="Y by X=c", xlab="Y", xlim=c(0,14), ylim=c(0,11), breaks=seq(0,14,2), col="lightgreen")
par(mfrow=c(1,1))

m = glm(Y ~ X, data=df, family=poisson)
print(check_overdispersion(m))
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 7. Zero-Inflated Poisson ####
##
# df has one between-Ss. factor (X) w/levels (a,b,c) and zero-inflated count response (Y)
set.seed(123)
a = round(rpois(20, lambda=4.50), digits=0)
b = round(rpois(20, lambda=5.50), digits=0)
c = round(rpois(20, lambda=5.00), digits=0)

a[sample(1:20, 5, replace=FALSE)] = 0
b[sample(1:20, 5, replace=FALSE)] = 0
c[sample(1:20, 5, replace=FALSE)] = 0

df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
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
boxplot(Y ~ X, data=df, main="Y by X", col=c("pink","lightblue","lightgreen"))

par(mfrow=c(3,1))
  hist(df[df$X == "a",]$Y, main="Y by X=a", xlab="Y", xlim=c(0,12), ylim=c(0,9), breaks=seq(0,12,2), col="pink")
  hist(df[df$X == "b",]$Y, main="Y by X=b", xlab="Y", xlim=c(0,12), ylim=c(0,9), breaks=seq(0,12,2), col="lightblue")
  hist(df[df$X == "c",]$Y, main="Y by X=c", xlab="Y", xlim=c(0,12), ylim=c(0,9), breaks=seq(0,12,2), col="lightgreen")
par(mfrow=c(1,1))

m0 = glm(Y ~ X, data=df, family=poisson)
print(check_zeroinflation(m0))

m = glmmTMB(Y ~ X, data=df, family=poisson, ziformula=~1)
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 8. Negative Binomial ####
##
# df has one between-Ss. factor (X) w/levels (a,b,c) and overdispersed count response (Y)
set.seed(123)
a = round(rnbinom(20, size=4.0, mu=2.0), digits=0)
b = round(rnbinom(20, size=5.0, mu=4.0), digits=0)
c = round(rnbinom(20, size=6.0, mu=3.0), digits=0)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
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
boxplot(Y ~ X, data=df, main="Y by X", col=c("pink","lightblue","lightgreen"))

par(mfrow=c(3,1))
  hist(df[df$X == "a",]$Y, main="Y by X=a", xlab="Y", xlim=c(0,10), ylim=c(0,12), breaks=seq(0,10,2), col="pink")
  hist(df[df$X == "b",]$Y, main="Y by X=b", xlab="Y", xlim=c(0,10), ylim=c(0,12), breaks=seq(0,10,2), col="lightblue")
  hist(df[df$X == "c",]$Y, main="Y by X=c", xlab="Y", xlim=c(0,10), ylim=c(0,12), breaks=seq(0,10,2), col="lightgreen")
par(mfrow=c(1,1))

m0 = glm(Y ~ X, data=df, family=poisson)
print(check_overdispersion(m0))

m = glm.nb(Y ~ X, data=df)
print(check_overdispersion(m))
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 9. Zero-Inflated Negative Binomial ####
##
# df has one between-Ss. factor (X) w/levels (a,b,c) and zero-inflated overdispersed count response (Y)
set.seed(123)
a = round(rnbinom(20, size=3.5, mu=2.0), digits=0)
b = round(rnbinom(20, size=6.5, mu=6.5), digits=0)
c = round(rnbinom(20, size=6.0, mu=3.0), digits=0)

a[sample(1:20, 5, replace=FALSE)] = 0
b[sample(1:20, 5, replace=FALSE)] = 0
c[sample(1:20, 5, replace=FALSE)] = 0

df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
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
boxplot(Y ~ X, data=df, main="Y by X", col=c("pink","lightblue","lightgreen"))

par(mfrow=c(3,1))
  hist(df[df$X == "a",]$Y, main="Y by X=a", xlab="Y", xlim=c(0,16), ylim=c(0,14), breaks=seq(0,16,2), col="pink")
  hist(df[df$X == "b",]$Y, main="Y by X=b", xlab="Y", xlim=c(0,16), ylim=c(0,14), breaks=seq(0,16,2), col="lightblue")
  hist(df[df$X == "c",]$Y, main="Y by X=c", xlab="Y", xlim=c(0,16), ylim=c(0,14), breaks=seq(0,16,2), col="lightgreen")
par(mfrow=c(1,1))

m0 = glm(Y ~ X, data=df, family=poisson)
print(check_overdispersion(m0))

m0 = glm.nb(Y ~ X, data=df)
print(check_zeroinflation(m0))

m = glmmTMB(Y ~ X, data=df, family=nbinom2, ziformula=~1)
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 10. Exponential ####
##
# df has one between-Ss. factor (X) w/levels (a,b,c) and exponential response (Y)
set.seed(123)
a = round(rexp(20, rate=1/6.00), digits=2)
b = round(rexp(20, rate=1/12.0), digits=2)
c = round(rexp(20, rate=1/4.50), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
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
boxplot(Y ~ X, data=df, main="Y by X", col=c("pink","lightblue","lightgreen"))

par(mfrow=c(3,1))
  hist(df[df$X == "a",]$Y, main="Y by X=a", xlab="Y", xlim=c(0,36), ylim=c(0,13), breaks=seq(0,36,4), col="pink")
  hist(df[df$X == "b",]$Y, main="Y by X=b", xlab="Y", xlim=c(0,36), ylim=c(0,13), breaks=seq(0,36,4), col="lightblue")
  hist(df[df$X == "c",]$Y, main="Y by X=c", xlab="Y", xlim=c(0,36), ylim=c(0,13), breaks=seq(0,36,4), col="lightgreen")
par(mfrow=c(1,1))

m0 = lm(Y ~ X, data=df)
print(check_normality(m0))
print(check_homogeneity(m0))

m = glm(Y ~ X, data=df, family=Gamma(link="log"))
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 11. Gamma ####
##
# df has one between-Ss. factor (X) w/levels (a,b,c) and skewed continuous response (Y)
set.seed(123)
a = round(rgamma(20, shape=5.0, scale=4.0), digits=2)
b = round(rgamma(20, shape=3.0, scale=3.0), digits=2)
c = round(rgamma(20, shape=4.0, scale=3.0), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
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
boxplot(Y ~ X, data=df, main="Y by X", col=c("pink","lightblue","lightgreen"))

par(mfrow=c(3,1))
  hist(df[df$X == "a",]$Y, main="Y by X=a", xlab="Y", xlim=c(0,36), ylim=c(0,8), breaks=seq(0,36,4), col="pink")
  hist(df[df$X == "b",]$Y, main="Y by X=b", xlab="Y", xlim=c(0,36), ylim=c(0,8), breaks=seq(0,36,4), col="lightblue")
  hist(df[df$X == "c",]$Y, main="Y by X=c", xlab="Y", xlim=c(0,36), ylim=c(0,8), breaks=seq(0,36,4), col="lightgreen")
par(mfrow=c(1,1))

m0 = lm(Y ~ X, data=df)
print(check_normality(m0))
print(check_homogeneity(m0))

m = glm(Y ~ X, data=df, family=Gamma)
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")




###
#### Multiple Between-Ss. Factors (2x2) ####
###

##
#### 12. Normal ####
##
# df has two between-Ss. factors (X1,X2) each w/levels (a,b) and continuous response (Y)
set.seed(123)
aa = round(rnorm(15, mean=30.0, sd=12.0), digits=2)
ab = round(rnorm(15, mean=45.0, sd=15.0), digits=2)
ba = round(rnorm(15, mean=40.0, sd=14.0), digits=2)
bb = round(rnorm(15, mean=35.0, sd=11.0), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
View(df)

ddply(df, ~ X1 + X2, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))
with(df, 
     interaction.plot(
       X1, 
       X2, 
       Y, 
       ylim=c(min(Y), max(Y)), 
       ylab="Y",
       main="Y by X1, X2",
       lty=1, 
       lwd=3, 
       col=c("red","blue"))
)
msd <- ddply(df, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

par(mfrow=c(4,1))
  hist(df[df$X1 == "a" & df$X2 == "a",]$Y, main="Y by (a,a)", xlab="Y", xlim=c(10,80), ylim=c(0,8), breaks=seq(10,80,10), col="pink")
  hist(df[df$X1 == "a" & df$X2 == "b",]$Y, main="Y by (a,b)", xlab="Y", xlim=c(10,80), ylim=c(0,8), breaks=seq(10,80,10), col="red")
  hist(df[df$X1 == "b" & df$X2 == "a",]$Y, main="Y by (b,a)", xlab="Y", xlim=c(10,80), ylim=c(0,8), breaks=seq(10,80,10), col="lightblue")
  hist(df[df$X1 == "b" & df$X2 == "b",]$Y, main="Y by (b,b)", xlab="Y", xlim=c(10,80), ylim=c(0,8), breaks=seq(10,80,10), col="blue")
par(mfrow=c(1,1))

m = lm(Y ~ X1*X2, data=df)
print(check_normality(m))
print(check_homogeneity(m))
Anova(m, type=3, test.statistic="F")
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 13. Lognormal ####
##
# df has two between-Ss. factors (X1,X2) each w/levels (a,b) and skewed continuous response (Y)
set.seed(123)
aa = round(rlnorm(15, meanlog=2.20, sdlog=0.80), digits=2)
ab = round(rlnorm(15, meanlog=2.50, sdlog=0.80), digits=2)
ba = round(rlnorm(15, meanlog=2.50, sdlog=0.80), digits=2)
bb = round(rlnorm(15, meanlog=2.20, sdlog=0.80), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
View(df)

ddply(df, ~ X1 + X2, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))
with(df, 
     interaction.plot(
       X1, 
       X2, 
       Y, 
       ylim=c(min(Y), max(Y)), 
       ylab="Y",
       main="Y by X1, X2",
       lty=1, 
       lwd=3, 
       col=c("red","blue"))
)
msd <- ddply(df, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

par(mfrow=c(4,1))
  hist(df[df$X1 == "a" & df$X2 == "a",]$Y, main="Y by (a,a)", xlab="Y", xlim=c(0,70), ylim=c(0,7), breaks=seq(0,70,5), col="pink")
  hist(df[df$X1 == "a" & df$X2 == "b",]$Y, main="Y by (a,b)", xlab="Y", xlim=c(0,70), ylim=c(0,7), breaks=seq(0,70,5), col="red")
  hist(df[df$X1 == "b" & df$X2 == "a",]$Y, main="Y by (b,a)", xlab="Y", xlim=c(0,70), ylim=c(0,7), breaks=seq(0,70,5), col="lightblue")
  hist(df[df$X1 == "b" & df$X2 == "b",]$Y, main="Y by (b,b)", xlab="Y", xlim=c(0,70), ylim=c(0,7), breaks=seq(0,70,5), col="blue")
par(mfrow=c(1,1))

m0 = lm(Y ~ X1*X2, data=df)
print(check_normality(m0))
print(check_homogeneity(m0))

m = lm(log(Y) ~ X1*X2, data=df)
print(check_normality(m))
print(check_homogeneity(m))
Anova(m, type=3, test.statistic="F")
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 14. Binomial ####
##
# df has two between-Ss. factors (X1,X2) each w/levels (a,b) and dichotomous response (Y)
set.seed(123)
aa = sample(c("no", "yes"), 15, replace=TRUE, prob=c(0.7, 0.3))
ab = sample(c("no", "yes"), 15, replace=TRUE, prob=c(0.2, 0.8))
ba = sample(c("no", "yes"), 15, replace=TRUE, prob=c(0.7, 0.3))
bb = sample(c("no", "yes"), 15, replace=TRUE, prob=c(0.4, 0.6))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = factor(c(aa,ab,ba,bb))
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
View(df)

ddply(df, ~ X1 + X2, function(data) c(
  "Nrows"=nrow(data),
  "no"=sum(data$Y == "no"),
  "yes"=sum(data$Y == "yes")
))
mosaicplot( ~ X1 + X2 + Y, data=df, main="Y by X", col=c("pink","lightgreen"))

m = glm(Y ~ X1*X2, data=df, family=binomial)
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 15. Multinomial ####
##
# df has two between-Ss. factors (X1,X2) each w/levels (a,b) and polytomous response (Y)
set.seed(123)
aa = sample(c("maybe","no","yes"), 15, replace=TRUE, prob=c(0.5, 0.3, 0.2))
ab = sample(c("maybe","no","yes"), 15, replace=TRUE, prob=c(0.3, 0.2, 0.5))
ba = sample(c("maybe","no","yes"), 15, replace=TRUE, prob=c(0.2, 0.5, 0.3))
bb = sample(c("maybe","no","yes"), 15, replace=TRUE, prob=c(0.2, 0.6, 0.2))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = factor(c(aa,ab,ba,bb))
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
View(df)

ddply(df, ~ X1 + X2, function(data) c(
  "Nrows"=nrow(data),
  "maybe"=sum(data$Y == "maybe"),
  "no"=sum(data$Y == "no"),
  "yes"=sum(data$Y == "yes")
))
mosaicplot( ~ X1 + X2 + Y, data=df, main="Y by X", col=c("lightyellow","pink","lightgreen"))

m0 = multinom(Y ~ X1 * X2, data=df, trace=FALSE)
Anova(m0, type=3)
# post hoc tests with emmeans are thorny for multinom models

m = glm.mp(Y ~ X1 * X2, data=df) # use the multinomial-Poisson trick
Anova.mp(m, type=3)
glm.mp.con(m, pairwise ~ X1*X2, adjust="holm")



##
#### 16. Ordinal ####
##
# df has two between-Ss. factors (X1,X2) each w/levels (a,b) and ordinal response (1-7)
set.seed(123)
aa = sample(1:7, 15, replace=TRUE, prob=c(0.10, 0.10, 0.20, 0.30, 0.40, 0.30, 0.20))
ab = sample(1:7, 15, replace=TRUE, prob=c(0.15, 0.10, 0.20, 0.20, 0.20, 0.30, 0.50))
ba = sample(1:7, 15, replace=TRUE, prob=c(0.10, 0.20, 0.20, 0.20, 0.20, 0.10, 0.05))
bb = sample(1:7, 15, replace=TRUE, prob=c(0.55, 0.60, 0.50, 0.50, 0.40, 0.35, 0.35))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
View(df)

ddply(df, ~ X1 + X2, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))
mosaicplot( ~ X1 + X2 + Y, data=df, main="Y by X1, X2", col=terrain.colors(7))

with(df, 
     interaction.plot(
       X1, 
       X2, 
       Y, 
       ylim=c(min(Y), max(Y)), 
       ylab="Y",
       main="Y by X1, X2",
       lty=1, 
       lwd=3, 
       col=c("red","blue"))
)
msd <- ddply(df, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

par(mfrow=c(4,1))
  hist(df[df$X1 == "a" & df$X2 == "a",]$Y, main="Y by (a,a)", xlab="Y", xlim=c(1,7), ylim=c(0,7), breaks=seq(1,7,1), col="pink")
  hist(df[df$X1 == "a" & df$X2 == "b",]$Y, main="Y by (a,b)", xlab="Y", xlim=c(1,7), ylim=c(0,7), breaks=seq(1,7,1), col="red")
  hist(df[df$X1 == "b" & df$X2 == "a",]$Y, main="Y by (b,a)", xlab="Y", xlim=c(1,7), ylim=c(0,7), breaks=seq(1,7,1), col="lightblue")
  hist(df[df$X1 == "b" & df$X2 == "b",]$Y, main="Y by (b,b)", xlab="Y", xlim=c(1,7), ylim=c(0,7), breaks=seq(1,7,1), col="blue")
par(mfrow=c(1,1))

df$Y = ordered(df$Y)
m = polr(Y ~ X1*X2, data=df, Hess=TRUE)
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 17. Poisson ####
##
# df has two between-Ss. factors (X1,X2) each w/levels (a,b) and count response (Y)
set.seed(123)
aa = round(rpois(15, lambda=4.00), digits=0)
ab = round(rpois(15, lambda=6.00), digits=0)
ba = round(rpois(15, lambda=5.00), digits=0)
bb = round(rpois(15, lambda=4.50), digits=0)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
View(df)

ddply(df, ~ X1 + X2, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))
with(df, 
     interaction.plot(
       X1, 
       X2, 
       Y, 
       ylim=c(min(Y), max(Y)), 
       ylab="Y",
       main="Y by X1, X2",
       lty=1, 
       lwd=3, 
       col=c("red","blue"))
)
msd <- ddply(df, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

par(mfrow=c(4,1))
  hist(df[df$X1 == "a" & df$X2 == "a",]$Y, main="Y by (a,a)", xlab="Y", xlim=c(0,14), ylim=c(0,8), breaks=seq(0,14,2), col="pink")
  hist(df[df$X1 == "a" & df$X2 == "b",]$Y, main="Y by (a,b)", xlab="Y", xlim=c(0,14), ylim=c(0,8), breaks=seq(0,14,2), col="red")
  hist(df[df$X1 == "b" & df$X2 == "a",]$Y, main="Y by (b,a)", xlab="Y", xlim=c(0,14), ylim=c(0,8), breaks=seq(0,14,2), col="lightblue")
  hist(df[df$X1 == "b" & df$X2 == "b",]$Y, main="Y by (b,b)", xlab="Y", xlim=c(0,14), ylim=c(0,8), breaks=seq(0,14,2), col="blue")
par(mfrow=c(1,1))

m = glm(Y ~ X1*X2, data=df, family=poisson)
print(check_overdispersion(m))
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 18. Zero-Inflated Poisson ####
##
# df has two between-Ss. factors (X1,X2) each w/levels (a,b) and zero-inflated count response (Y)
set.seed(123)
aa = round(rpois(15, lambda=4.00), digits=0)
ab = round(rpois(15, lambda=6.50), digits=0)
ba = round(rpois(15, lambda=5.00), digits=0)
bb = round(rpois(15, lambda=4.50), digits=0)

aa[sample(1:15, 4, replace=FALSE)] = 0
ab[sample(1:15, 4, replace=FALSE)] = 0
ba[sample(1:15, 4, replace=FALSE)] = 0
bb[sample(1:15, 4, replace=FALSE)] = 0

df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
View(df)

ddply(df, ~ X1 + X2, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))
with(df, 
     interaction.plot(
       X1, 
       X2, 
       Y, 
       ylim=c(min(Y), max(Y)), 
       ylab="Y",
       main="Y by X1, X2",
       lty=1, 
       lwd=3, 
       col=c("red","blue"))
)
msd <- ddply(df, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

par(mfrow=c(4,1))
  hist(df[df$X1 == "a" & df$X2 == "a",]$Y, main="Y by (a,a)", xlab="Y", xlim=c(0,14), ylim=c(0,7), breaks=seq(0,14,2), col="pink")
  hist(df[df$X1 == "a" & df$X2 == "b",]$Y, main="Y by (a,b)", xlab="Y", xlim=c(0,14), ylim=c(0,7), breaks=seq(0,14,2), col="red")
  hist(df[df$X1 == "b" & df$X2 == "a",]$Y, main="Y by (b,a)", xlab="Y", xlim=c(0,14), ylim=c(0,7), breaks=seq(0,14,2), col="lightblue")
  hist(df[df$X1 == "b" & df$X2 == "b",]$Y, main="Y by (b,b)", xlab="Y", xlim=c(0,14), ylim=c(0,7), breaks=seq(0,14,2), col="blue")
par(mfrow=c(1,1))

m0 = glm(Y ~ X1*X2, data=df, family=poisson)
print(check_zeroinflation(m0))

m = glmmTMB(Y ~ X1*X2, data=df, family=poisson, ziformula=~1)
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 19. Negative Binomial ####
##
# df has two between-Ss. factors (X1,X2) each w/levels (a,b) and overdispersed count response (Y)
set.seed(123)
aa = round(rnbinom(15, size=7.0, mu=6.0), digits=0)
ab = round(rnbinom(15, size=3.0, mu=4.5), digits=0)
ba = round(rnbinom(15, size=4.5, mu=4.5), digits=0)
bb = round(rnbinom(15, size=5.0, mu=3.5), digits=0)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
View(df)

ddply(df, ~ X1 + X2, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))
with(df, 
     interaction.plot(
       X1, 
       X2, 
       Y, 
       ylim=c(min(Y), max(Y)), 
       ylab="Y",
       main="Y by X1, X2",
       lty=1, 
       lwd=3, 
       col=c("red","blue"))
)
msd <- ddply(df, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

par(mfrow=c(4,1))
  hist(df[df$X1 == "a" & df$X2 == "a",]$Y, main="Y by (a,a)", xlab="Y", xlim=c(0,14), ylim=c(0,8), breaks=seq(0,14,2), col="pink")
  hist(df[df$X1 == "a" & df$X2 == "b",]$Y, main="Y by (a,b)", xlab="Y", xlim=c(0,14), ylim=c(0,8), breaks=seq(0,14,2), col="red")
  hist(df[df$X1 == "b" & df$X2 == "a",]$Y, main="Y by (b,a)", xlab="Y", xlim=c(0,14), ylim=c(0,8), breaks=seq(0,14,2), col="lightblue")
  hist(df[df$X1 == "b" & df$X2 == "b",]$Y, main="Y by (b,b)", xlab="Y", xlim=c(0,14), ylim=c(0,8), breaks=seq(0,14,2), col="blue")
par(mfrow=c(1,1))

m0 = glm(Y ~ X1*X2, data=df, family=poisson)
print(check_overdispersion(m0))

m = glm.nb(Y ~ X1*X2, data=df)
print(check_overdispersion(m))
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 20. Zero-Inflated Negative Binomial ####
##
# df has two between-Ss. factors (X1,X2) each w/levels (a,b) and zero-inflated overdispersed count response (Y)
set.seed(123)
aa = round(rnbinom(15, size=8.0, mu=6.0), digits=0)
ab = round(rnbinom(15, size=3.0, mu=4.5), digits=0)
ba = round(rnbinom(15, size=4.5, mu=4.5), digits=0)
bb = round(rnbinom(15, size=5.0, mu=3.5), digits=0)

aa[sample(1:15, 4, replace=FALSE)] = 0
ab[sample(1:15, 4, replace=FALSE)] = 0
ba[sample(1:15, 4, replace=FALSE)] = 0
bb[sample(1:15, 4, replace=FALSE)] = 0

df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
View(df)

ddply(df, ~ X1 + X2, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))
with(df, 
     interaction.plot(
       X1, 
       X2, 
       Y, 
       ylim=c(min(Y), max(Y)), 
       ylab="Y",
       main="Y by X1, X2",
       lty=1, 
       lwd=3, 
       col=c("red","blue"))
)
msd <- ddply(df, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

par(mfrow=c(4,1))
  hist(df[df$X1 == "a" & df$X2 == "a",]$Y, main="Y by (a,a)", xlab="Y", xlim=c(0,14), ylim=c(0,7), breaks=seq(0,14,2), col="pink")
  hist(df[df$X1 == "a" & df$X2 == "b",]$Y, main="Y by (a,b)", xlab="Y", xlim=c(0,14), ylim=c(0,7), breaks=seq(0,14,2), col="red")
  hist(df[df$X1 == "b" & df$X2 == "a",]$Y, main="Y by (b,a)", xlab="Y", xlim=c(0,14), ylim=c(0,7), breaks=seq(0,14,2), col="lightblue")
  hist(df[df$X1 == "b" & df$X2 == "b",]$Y, main="Y by (b,b)", xlab="Y", xlim=c(0,14), ylim=c(0,7), breaks=seq(0,14,2), col="blue")
par(mfrow=c(1,1))

m0 = glm(Y ~ X1*X2, data=df, family=poisson)
print(check_overdispersion(m0))

m0 = glm.nb(Y ~ X1*X2, data=df)
print(check_zeroinflation(m0))

m = glmmTMB(Y ~ X1*X2, data=df, family=nbinom2, ziformula=~1)
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 21. Exponential ####
##
# df has two between-Ss. factors (X1,X2) each w/levels (a,b) and exponential response (Y)
set.seed(123)
aa = round(rexp(15, rate=1/6.00), digits=2)
ab = round(rexp(15, rate=1/12.0), digits=2)
ba = round(rexp(15, rate=1/4.50), digits=2)
bb = round(rexp(15, rate=1/14.0), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
View(df)

ddply(df, ~ X1 + X2, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))
with(df, 
     interaction.plot(
       X1, 
       X2, 
       Y, 
       ylim=c(min(Y), max(Y)), 
       ylab="Y",
       main="Y by X1, X2",
       lty=1, 
       lwd=3, 
       col=c("red","blue"))
)
msd <- ddply(df, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

par(mfrow=c(4,1))
  hist(df[df$X1 == "a" & df$X2 == "a",]$Y, main="Y by (a,a)", xlab="Y", xlim=c(0,50), ylim=c(0,11), breaks=seq(0,50,5), col="pink")
  hist(df[df$X1 == "a" & df$X2 == "b",]$Y, main="Y by (a,b)", xlab="Y", xlim=c(0,50), ylim=c(0,11), breaks=seq(0,50,5), col="red")
  hist(df[df$X1 == "b" & df$X2 == "a",]$Y, main="Y by (b,a)", xlab="Y", xlim=c(0,50), ylim=c(0,11), breaks=seq(0,50,5), col="lightblue")
  hist(df[df$X1 == "b" & df$X2 == "b",]$Y, main="Y by (b,b)", xlab="Y", xlim=c(0,50), ylim=c(0,11), breaks=seq(0,50,5), col="blue")
par(mfrow=c(1,1))

m0 = lm(Y ~ X1*X2, data=df)
print(check_normality(m0))
print(check_homogeneity(m0))

m = glm(Y ~ X1*X2, data=df, family=Gamma(link="log"))
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 22. Gamma ####
##
# df has two between-Ss. factors (X1,X2) each w/levels (a,b) and skewed continuous response (Y)
set.seed(123)
aa = round(rgamma(15, shape=6.5, scale=3.5), digits=2)
ab = round(rgamma(15, shape=4.0, scale=3.5), digits=2)
ba = round(rgamma(15, shape=5.5, scale=4.5), digits=2)
bb = round(rgamma(15, shape=6.0, scale=2.5), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
View(df)

ddply(df, ~ X1 + X2, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))
with(df, 
     interaction.plot(
       X1, 
       X2, 
       Y, 
       ylim=c(min(Y), max(Y)), 
       ylab="Y",
       main="Y by X1, X2",
       lty=1, 
       lwd=3, 
       col=c("red","blue"))
)
msd <- ddply(df, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

par(mfrow=c(4,1))
  hist(df[df$X1 == "a" & df$X2 == "a",]$Y, main="Y by (a,a)", xlab="Y", xlim=c(0,40), ylim=c(0,7), breaks=seq(0,40,5), col="pink")
  hist(df[df$X1 == "a" & df$X2 == "b",]$Y, main="Y by (a,b)", xlab="Y", xlim=c(0,40), ylim=c(0,7), breaks=seq(0,40,5), col="red")
  hist(df[df$X1 == "b" & df$X2 == "a",]$Y, main="Y by (b,a)", xlab="Y", xlim=c(0,40), ylim=c(0,7), breaks=seq(0,40,5), col="lightblue")
  hist(df[df$X1 == "b" & df$X2 == "b",]$Y, main="Y by (b,b)", xlab="Y", xlim=c(0,40), ylim=c(0,7), breaks=seq(0,40,5), col="blue")
par(mfrow=c(1,1))

m0 = lm(Y ~ X1*X2, data=df)
print(check_normality(m0))
print(check_homogeneity(m0))

m = glm(Y ~ X1*X2, data=df, family=Gamma)
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")


