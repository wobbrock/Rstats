###
### Statistical Inference in R
### Jacob O. Wobbrock, Ph.D.
### wobbrock@uw.edu
### The Information School
### University of Washington
### March 12, 2019
### Updated: 3/13/2025
###

###
### GLMM.R
### (Generalized linear mixed models)
###

library(plyr) # for ddply
library(lme4) # for lmer, glmer, glmer.nb
library(lmerTest)
library(car) # for Anova
library(emmeans) # for emmeans
library(multpois) # for glmer.mp, Anova.mp, glmer.mp.con
library(ordinal) # for clmm
library(RVAideMemoire) # for Anova.clmm
library(performance) # for check_*
library(glmmTMB) # for glmmTMB


###
#### One Within-Ss. Factor (3 levels) ####
###

##
#### 1. Normal ####
##
# df has one within-Ss. factor (X) w/levels (a,b,c) and continuous response (Y)
set.seed(123)
a = round(rnorm(20, mean=30.0, sd=12.0), digits=2)
b = round(rnorm(20, mean=45.0, sd=15.0), digits=2)
c = round(rnorm(20, mean=40.0, sd=14.0), digits=2)
df = data.frame(
  PId = factor(rep(1:20, times=3)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
)
contrasts(df$X) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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

m = lmer(Y ~ X + (1|PId), data=df)
print(check_normality(m))
print(check_homogeneity(m))
Anova(m, type=3, test.statistic="F")
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 2. Lognormal ####
##
# df has one within-Ss. factor (X) w/levels (a,b,c) and skewed continuous response (Y)
set.seed(123)
a = round(rlnorm(20, meanlog=2.30, sdlog=0.90), digits=2)
b = round(rlnorm(20, meanlog=2.90, sdlog=1.00), digits=2)
c = round(rlnorm(20, meanlog=1.95, sdlog=1.10), digits=2)
df = data.frame(
  PId = factor(rep(1:20, times=3)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
)
contrasts(df$X) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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

m0 = lmer(Y ~ X + (1|PId), data=df)
print(check_normality(m0))
print(check_homogeneity(m0))

m = lmer(log(Y) ~ X + (1|PId), data=df)
print(check_normality(m))
print(check_homogeneity(m))
Anova(m, type=3, test.statistic="F")
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 3. Binomial ####
##
# df has one within-Ss. factor (X) w/levels (a,b,c) and dichotomous response (Y)
set.seed(123)
a = sample(c("yes", "no"), 20, replace=TRUE, prob=c(0.7, 0.3))
b = sample(c("yes", "no"), 20, replace=TRUE, prob=c(0.3, 0.7))
c = sample(c("yes", "no"), 20, replace=TRUE, prob=c(0.5, 0.5))
df = data.frame(
  PId = factor(rep(1:20, times=3)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = factor(c(a,b,c), levels=c("yes","no"))
)
contrasts(df$X) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "yes"=sum(data$Y == "yes"),
  "no"=sum(data$Y == "no")
))
mosaicplot( ~ X + Y, data=df, main="Y by X", col=c("lightgreen","pink"))

m = glmer(Y ~ X + (1|PId), data=df, family=binomial)
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 4. Multinomial ####
##
# df has one within-Ss. factor (X) w/levels (a,b,c) and polytomous response (Y)
set.seed(123)
a = sample(c("yes","no","maybe"), 20, replace=TRUE, prob=c(0.4, 0.5, 0.1))
b = sample(c("yes","no","maybe"), 20, replace=TRUE, prob=c(0.5, 0.3, 0.2))
c = sample(c("yes","no","maybe"), 20, replace=TRUE, prob=c(0.3, 0.2, 0.5))
df = data.frame(
  PId = factor(rep(1:20, times=3)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = factor(c(a,b,c), levels=c("yes","no","maybe"))
)
contrasts(df$X) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "yes"=sum(data$Y == "yes"),
  "no"=sum(data$Y == "no"),
  "maybe"=sum(data$Y == "maybe")
))
mosaicplot( ~ X + Y, data=df, main="Y by X", col=c("lightgreen","pink","lightyellow"))

# use the multinomial-Poisson trick
m = glmer.mp(Y ~ X + (1|PId), data=df)
Anova.mp(m, type=3)
glmer.mp.con(m, pairwise ~ X, adjust="holm", control=glmerControl(optimizer="bobyqa"))



##
#### 5. Ordinal ####
##
# dt has one within-Ss. factor (X) w/levels (a,b,c) and ordinal response (1-7)
set.seed(123)
a = laply(round(rnorm(20, mean=4.15, sd=1.55), digits=0), function(x) min(max(x, 1), 7))
b = laply(round(rnorm(20, mean=4.55, sd=1.65), digits=0), function(x) min(max(x, 1), 7))
c = laply(round(rnorm(20, mean=3.20, sd=1.50), digits=0), function(x) min(max(x, 1), 7))
dt = data.frame( # name table 'dt' to avoid Anova.clmm bug (see below)
  PId = factor(rep(1:20, times=3)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
)
contrasts(dt$X) <- "contr.sum"
dt <- dt[order(dt$PId),] # sort by PId
row.names(dt) <- 1:nrow(dt) # restore row numbers
View(dt)

ddply(dt, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))
mosaicplot( ~ X + Y, data=dt, main="Y by X", col=terrain.colors(7))

boxplot(Y ~ X, data=dt, main="Y by X", col=c("pink","lightblue","lightgreen"))

par(mfrow=c(3,1))
  hist(dt[dt$X == "a",]$Y, main="Y by X=a", xlab="Y", xlim=c(1,7), ylim=c(0,8), breaks=seq(1,7,1), col="pink")
  hist(dt[dt$X == "b",]$Y, main="Y by X=b", xlab="Y", xlim=c(1,7), ylim=c(0,8), breaks=seq(1,7,1), col="lightblue")
  hist(dt[dt$X == "c",]$Y, main="Y by X=c", xlab="Y", xlim=c(1,7), ylim=c(0,8), breaks=seq(1,7,1), col="lightgreen")
par(mfrow=c(1,1))

dt$Y = ordered(dt$Y)
# Anova.clmm fails if 'data' is named 'df'; we're using 'dt' instead
m = clmm(Y ~ X + (1|PId), data=dt, Hess=TRUE, link="logit")
# "logit", "probit", "cloglog", "loglog", and "cauchit" links are options
Anova.clmm(m)
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 6. Poisson ####
##
# df has one within-Ss. factor (X) w/levels (a,b,c) and count response (Y)
set.seed(123)
a = round(rpois(20, lambda=4.00), digits=0)
b = round(rpois(20, lambda=6.00), digits=0)
c = round(rpois(20, lambda=5.00), digits=0)
df = data.frame(
  PId = factor(rep(1:20, times=3)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
)
contrasts(df$X) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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

m = glmer(Y ~ X + (1|PId), data=df, family=poisson)
print(check_overdispersion(m))
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 7. Zero-Inflated Poisson ####
##
# df has one within-Ss. factor (X) w/levels (a,b,c) and zero-inflated count response (Y)
set.seed(123)
a = round(rpois(20, lambda=4.50), digits=0)
b = round(rpois(20, lambda=5.50), digits=0)
c = round(rpois(20, lambda=5.00), digits=0)

a[sample(1:20, 5, replace=FALSE)] = 0
b[sample(1:20, 5, replace=FALSE)] = 0
c[sample(1:20, 5, replace=FALSE)] = 0

df = data.frame(
  PId = factor(rep(1:20, times=3)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
)
contrasts(df$X) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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

m0 = glmer(Y ~ X + (1|PId), data=df, family=poisson)
print(check_zeroinflation(m0))

m = glmmTMB(Y ~ X + (1|PId), data=df, family=poisson, ziformula=~1, REML=TRUE)
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 8. Negative Binomial ####
##
# df has one within-Ss. factor (X) w/levels (a,b,c) and overdispersed count response (Y)
set.seed(123)
a = round(rnbinom(20, size=4.0, mu=2.0), digits=0)
b = round(rnbinom(20, size=5.0, mu=4.0), digits=0)
c = round(rnbinom(20, size=6.0, mu=3.0), digits=0)
df = data.frame(
  PId = factor(rep(1:20, times=3)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
)
contrasts(df$X) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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

m0 = glmer(Y ~ X + (1|PId), data=df, family=poisson)
print(check_overdispersion(m0))

m = glmer.nb(Y ~ X + (1|PId), data=df)
print(check_overdispersion(m))
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 9. Zero-Inflated Negative Binomial ####
##
# df has one within-Ss. factor (X) w/levels (a,b,c) and zero-inflated overdispersed count response (Y)
set.seed(123)
a = round(rnbinom(20, size=3.5, mu=2.0), digits=0)
b = round(rnbinom(20, size=6.5, mu=6.5), digits=0)
c = round(rnbinom(20, size=6.0, mu=3.0), digits=0)

a[sample(1:20, 5, replace=FALSE)] = 0
b[sample(1:20, 5, replace=FALSE)] = 0
c[sample(1:20, 5, replace=FALSE)] = 0

df = data.frame(
  PId = factor(rep(1:20, times=3)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
)
contrasts(df$X) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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

m0 = glmer(Y ~ X + (1|PId), data=df, family=poisson)
print(check_overdispersion(m0))

m0 = glmer.nb(Y ~ X + (1|PId), data=df)
print(check_zeroinflation(m0))

m = glmmTMB(Y ~ X + (1|PId), data=df, family=nbinom2, ziformula=~1, REML=TRUE)
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 10. Exponential ####
##
# df has one within-Ss. factor (X) w/levels (a,b,c) and exponential response (Y)
set.seed(123)
a = round(rexp(20, rate=1/6.00), digits=2)
b = round(rexp(20, rate=1/12.0), digits=2)
c = round(rexp(20, rate=1/4.50), digits=2)
df = data.frame(
  PId = factor(rep(1:20, times=3)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
)
contrasts(df$X) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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

m0 = lmer(Y ~ X + (1|PId), data=df)
print(check_normality(m0))
print(check_homogeneity(m0))

m = glmer(Y ~ X + (1|PId), data=df, family=Gamma(link="log"))
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")



##
#### 11. Gamma ####
##
# df has one within-Ss. factor (X) w/levels (a,b,c) and skewed continuous response (Y)
set.seed(123)
a = round(rgamma(20, shape=5.0, scale=4.0), digits=2)
b = round(rgamma(20, shape=3.0, scale=3.0), digits=2)
c = round(rgamma(20, shape=4.0, scale=3.0), digits=2)
df = data.frame(
  PId = factor(rep(1:20, times=3)),
  X = factor(rep(c("a","b","c"), each=20)),
  Y = c(a,b,c)
)
contrasts(df$X) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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

m0 = lmer(Y ~ X + (1|PId), data=df)
print(check_normality(m0))
print(check_homogeneity(m0))

m = glmer(Y ~ X + (1|PId), data=df, family=Gamma)
Anova(m, type=3)
emmeans(m, pairwise ~ X, adjust="holm")



###
#### Multiple Within-Ss. Factors (2x2) ####
###

##
#### 12. Normal ####
##
# df has two within-Ss. factors (X1,X2) each w/levels (a,b) and continuous response (Y)
set.seed(123)
aa = round(rnorm(15, mean=30.0, sd=12.0), digits=2)
ab = round(rnorm(15, mean=45.0, sd=15.0), digits=2)
ba = round(rnorm(15, mean=40.0, sd=14.0), digits=2)
bb = round(rnorm(15, mean=35.0, sd=11.0), digits=2)
df = data.frame(
  PId = factor(rep(1:15, times=4)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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
with(df, interaction.plot(
  X1, 
  X2, 
  Y, 
  ylim=c(min(Y), max(Y)), 
  ylab="Y",
  main="Y by X1, X2",
  lty=1, 
  lwd=3, 
  col=c("red","blue")
))
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

m = lmer(Y ~ X1*X2 + (1|PId), data=df)
print(check_normality(m))
print(check_homogeneity(m))
Anova(m, type=3, test.statistic="F")
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 13. Lognormal ####
##
# df has two within-Ss. factors (X1,X2) each w/levels (a,b) and skewed continuous response (Y)
set.seed(123)
aa = round(rlnorm(15, meanlog=2.20, sdlog=0.80), digits=2)
ab = round(rlnorm(15, meanlog=2.50, sdlog=0.80), digits=2)
ba = round(rlnorm(15, meanlog=2.50, sdlog=0.80), digits=2)
bb = round(rlnorm(15, meanlog=2.20, sdlog=0.80), digits=2)
df = data.frame(
  PId = factor(rep(1:15, times=4)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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
with(df, interaction.plot(
  X1, 
  X2, 
  Y, 
  ylim=c(min(Y), max(Y)), 
  ylab="Y",
  main="Y by X1, X2",
  lty=1, 
  lwd=3, 
  col=c("red","blue")
))
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

m0 = lmer(Y ~ X1*X2 + (1|PId), data=df)
print(check_normality(m0))
print(check_homogeneity(m0))

m = lmer(log(Y) ~ X1*X2 + (1|PId), data=df)
print(check_normality(m))
print(check_homogeneity(m))
Anova(m, type=3, test.statistic="F")
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 14. Binomial ####
##
# df has two within-Ss. factors (X1,X2) each w/levels (a,b) and dichotomous response (Y)
set.seed(123)
aa = sample(c("yes", "no"), 15, replace=TRUE, prob=c(0.7, 0.3))
ab = sample(c("yes", "no"), 15, replace=TRUE, prob=c(0.2, 0.8))
ba = sample(c("yes", "no"), 15, replace=TRUE, prob=c(0.7, 0.3))
bb = sample(c("yes", "no"), 15, replace=TRUE, prob=c(0.4, 0.6))
df = data.frame(
  PId = factor(rep(1:15, times=4)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = factor(c(aa,ab,ba,bb), levels=c("yes","no"))
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
View(df)

ddply(df, ~ X1 + X2, function(data) c(
  "Nrows"=nrow(data),
  "yes"=sum(data$Y == "yes"),
  "no"=sum(data$Y == "no")
))
mosaicplot( ~ X1 + X2 + Y, data=df, main="Y by X1, X2", col=c("lightgreen","pink"))

m = glmer(Y ~ X1*X2 + (1|PId), data=df, family=binomial)
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 15. Multinomial ####
##
# df has two within-Ss. factors (X1,X2) each w/levels (a,b) and polytomous response (Y)
set.seed(123)
aa = sample(c("yes","no","maybe"), 15, replace=TRUE, prob=c(0.5, 0.3, 0.2))
ab = sample(c("yes","no","maybe"), 15, replace=TRUE, prob=c(0.3, 0.2, 0.5))
ba = sample(c("yes","no","maybe"), 15, replace=TRUE, prob=c(0.2, 0.5, 0.3))
bb = sample(c("yes","no","maybe"), 15, replace=TRUE, prob=c(0.2, 0.6, 0.2))
df = data.frame(
  PId = factor(rep(1:15, times=4)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = factor(c(aa,ab,ba,bb), levels=c("yes","no","maybe"))
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
View(df)

ddply(df, ~ X1 + X2, function(data) c(
  "Nrows"=nrow(data),
  "yes"=sum(data$Y == "yes"),
  "no"=sum(data$Y == "no"),
  "maybe"=sum(data$Y == "maybe")
))
mosaicplot( ~ X1 + X2 + Y, data=df, main="Y by X1, X2", col=c("lightgreen","pink","lightyellow"))

# use the multinomial-Poisson trick
m = glmer.mp(Y ~ X1*X2 + (1|PId), data=df)
Anova.mp(m, type=3)
glmer.mp.con(m, pairwise ~ X1*X2, adjust="holm", control=glmerControl(optimizer="bobyqa"))



##
#### 16. Ordinal ####
##
# dt has two within-Ss. factors (X1,X2) each w/levels (a,b) and ordinal response (1-7)
set.seed(123)
aa = laply(round(rnorm(15, mean=5.25, sd=1.00), digits=0), function(x) min(max(x, 1), 7))
ab = laply(round(rnorm(15, mean=4.15, sd=1.25), digits=0), function(x) min(max(x, 1), 7))
ba = laply(round(rnorm(15, mean=2.95, sd=1.35), digits=0), function(x) min(max(x, 1), 7))
bb = laply(round(rnorm(15, mean=3.35, sd=1.30), digits=0), function(x) min(max(x, 1), 7))
dt = data.frame( # name table 'dt' to avoid Anova.clmm bug (see below)
  PId = factor(rep(1:15, times=4)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(dt$X1) <- "contr.sum"
contrasts(dt$X2) <- "contr.sum"
dt <- dt[order(dt$PId),] # sort by PId
row.names(dt) <- 1:nrow(dt) # restore row numbers
View(dt)

ddply(dt, ~ X1 + X2, function(data) c(
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
))
mosaicplot( ~ X1 + X2 + Y, data=dt, main="Y by X1, X2", col=terrain.colors(7))

with(dt, interaction.plot(
  X1, 
  X2, 
  Y, 
  ylim=c(min(Y), max(Y)), 
  ylab="Y",
  main="Y by X1, X2",
  lty=1, 
  lwd=3, 
  col=c("red","blue")
))
msd <- ddply(dt, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

par(mfrow=c(4,1))
  hist(dt[dt$X1 == "a" & dt$X2 == "a",]$Y, main="Y by (a,a)", xlab="Y", xlim=c(1,7), ylim=c(0,8), breaks=seq(1,7,1), col="pink")
  hist(dt[dt$X1 == "a" & dt$X2 == "b",]$Y, main="Y by (a,b)", xlab="Y", xlim=c(1,7), ylim=c(0,8), breaks=seq(1,7,1), col="red")
  hist(dt[dt$X1 == "b" & dt$X2 == "a",]$Y, main="Y by (b,a)", xlab="Y", xlim=c(1,7), ylim=c(0,8), breaks=seq(1,7,1), col="lightblue")
  hist(dt[dt$X1 == "b" & dt$X2 == "b",]$Y, main="Y by (b,b)", xlab="Y", xlim=c(1,7), ylim=c(0,8), breaks=seq(1,7,1), col="blue")
par(mfrow=c(1,1))

dt$Y = ordered(dt$Y)
# Anova.clmm fails if 'data' is named 'df'; we're using 'dt' instead
m = clmm(Y ~ X1*X2 + (1|PId), data=dt, Hess=TRUE, link="logit", control=clmm.control(method="ucminf"))
# "logit", "probit", "cloglog", "loglog", and "cauchit" links are options
Anova.clmm(m)
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 17. Poisson ####
##
# df has two within-Ss. factors (X1,X2) each w/levels (a,b) and count response (Y)
set.seed(123)
aa = round(rpois(15, lambda=4.00), digits=0)
ab = round(rpois(15, lambda=6.00), digits=0)
ba = round(rpois(15, lambda=5.00), digits=0)
bb = round(rpois(15, lambda=4.50), digits=0)
df = data.frame(
  PId = factor(rep(1:15, times=4)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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
with(df, interaction.plot(
  X1, 
  X2, 
  Y, 
  ylim=c(min(Y), max(Y)), 
  ylab="Y",
  main="Y by X1, X2",
  lty=1, 
  lwd=3, 
  col=c("red","blue")
))
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

m = glmer(Y ~ X1*X2 + (1|PId), data=df, family=poisson)
print(check_overdispersion(m))
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 18. Zero-Inflated Poisson ####
##
# df has two within-Ss. factors (X1,X2) each w/levels (a,b) and zero-inflated count response (Y)
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
  PId = factor(rep(1:15, times=4)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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
with(df, interaction.plot(
  X1, 
  X2, 
  Y, 
  ylim=c(min(Y), max(Y)), 
  ylab="Y",
  main="Y by X1, X2",
  lty=1, 
  lwd=3, 
  col=c("red","blue")
))
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

m0 = glmer(Y ~ X1*X2 + (1|PId), data=df, family=poisson)
print(check_zeroinflation(m0))

m = glmmTMB(Y ~ X1*X2 + (1|PId), data=df, family=poisson, ziformula=~1, REML=TRUE)
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 19. Negative Binomial ####
##
# df has two within-Ss. factors (X1,X2) each w/levels (a,b) and overdispersed count response (Y)
set.seed(123)
aa = round(rnbinom(15, size=7.0, mu=6.0), digits=0)
ab = round(rnbinom(15, size=3.0, mu=4.5), digits=0)
ba = round(rnbinom(15, size=4.5, mu=4.5), digits=0)
bb = round(rnbinom(15, size=5.0, mu=3.5), digits=0)
df = data.frame(
  PId = factor(rep(1:15, times=4)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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
with(df, interaction.plot(
  X1, 
  X2, 
  Y, 
  ylim=c(min(Y), max(Y)), 
  ylab="Y",
  main="Y by X1, X2",
  lty=1, 
  lwd=3, 
  col=c("red","blue")
))
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

m0 = glmer(Y ~ X1*X2 + (1|PId), data=df, family=poisson)
print(check_overdispersion(m0))

m = glmer.nb(Y ~ X1*X2 + (1|PId), data=df)
print(check_overdispersion(m))
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 20. Zero-Inflated Negative Binomial ####
##
# df has two within-Ss. factors (X1,X2) each w/levels (a,b) and zero-inflated overdispersed count response (Y)
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
  PId = factor(rep(1:15, times=4)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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
with(df, interaction.plot(
  X1, 
  X2, 
  Y, 
  ylim=c(min(Y), max(Y)), 
  ylab="Y",
  main="Y by X1, X2",
  lty=1, 
  lwd=3, 
  col=c("red","blue")
))
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

m0 = glmer(Y ~ X1*X2 + (1|PId), data=df, family=poisson)
print(check_overdispersion(m0))

m0 = glmer.nb(Y ~ X1*X2 + (1|PId), data=df)
print(check_zeroinflation(m0))

m = glmmTMB(Y ~ X1*X2 + (1|PId), data=df, family=nbinom2, ziformula=~1, REML=TRUE)
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 21. Exponential ####
##
# df has two within-Ss. factors (X1,X2) each w/levels (a,b) and exponential response (Y)
set.seed(123)
aa = round(rexp(15, rate=1/6.00), digits=2)
ab = round(rexp(15, rate=1/12.0), digits=2)
ba = round(rexp(15, rate=1/4.50), digits=2)
bb = round(rexp(15, rate=1/14.0), digits=2)
df = data.frame(
  PId = factor(rep(1:15, times=4)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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
with(df, interaction.plot(
  X1, 
  X2, 
  Y, 
  ylim=c(min(Y), max(Y)), 
  ylab="Y",
  main="Y by X1, X2",
  lty=1, 
  lwd=3, 
  col=c("red","blue")
))
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

m0 = lmer(Y ~ X1*X2 + (1|PId), data=df)
print(check_normality(m0))
print(check_homogeneity(m0))

m = glmer(Y ~ X1*X2 + (1|PId), data=df, family=Gamma(link="log"))
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")



##
#### 22. Gamma ####
##
# df has two within-Ss. factors (X1,X2) each w/levels (a,b) and skewed continuous response (Y)
set.seed(123)
aa = round(rgamma(15, shape=6.5, scale=3.5), digits=2)
ab = round(rgamma(15, shape=4.0, scale=3.5), digits=2)
ba = round(rgamma(15, shape=5.5, scale=4.5), digits=2)
bb = round(rgamma(15, shape=6.0, scale=2.5), digits=2)
df = data.frame(
  PId = factor(rep(1:15, times=4)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # restore row numbers
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
with(df, interaction.plot(
  X1, 
  X2, 
  Y, 
  ylim=c(min(Y), max(Y)), 
  ylab="Y",
  main="Y by X1, X2",
  lty=1, 
  lwd=3, 
  col=c("red","blue")
))
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

m0 = lmer(Y ~ X1*X2 + (1|PId), data=df)
print(check_normality(m0))
print(check_homogeneity(m0))

m = glmer(Y ~ X1*X2 + (1|PId), data=df, family=Gamma, nAGQ=0)
Anova(m, type=3)
emmeans(m, pairwise ~ X1*X2, adjust="holm")


