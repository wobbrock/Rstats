###
### Statistical Inference in R
### Jacob O. Wobbrock, Ph.D.
### wobbrock@uw.edu
### The Information School
### University of Washington
### March 12, 2019
### Updated: 8/13/2025
###

###
### Assumptions.R
### (Tests of ANOVA assumptions: Normality, homoscedasticity, sphericity)
###

library(plyr) # for ddply
library(afex) # for aov_ez
library(performance) # for check_*
library(EnvStats) # for gofTest, print.gof
library(lme4) # for lmer
library(lmerTest)
library(car) # for Anova
library(effectsize) # for cohens_d, eta_squared


###
#### 1. Normality ####
###

##
## Shapiro-Wilk normality tests
## (on the conditional response)
# 
# df has two factors (X1,X2) each w/two levels (a,b) and continuous response (Y)
set.seed(123)
aa = round(rnorm(15, 30.0, 10.0), digits=2)
ab = round(rnorm(15, 40.0, 12.0), digits=2)
ba = round(rnorm(15, 25.0, 7.5), digits=2)
bb = round(rnorm(15, 20.0, 5.0), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
View(df)

msd <- ddply(df, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
)); print(msd)

with(df, interaction.plot(
  X1, 
  X2, 
  Y, 
  ylim=c(min(msd$Mean - msd$SD), max(msd$Mean + msd$SD)), 
  ylab="Y",
  main="Y by X1, X2",
  lty=1, 
  lwd=3, 
  col=c("red","blue")
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

shapiro.test(df[df$X1 == "a" & df$X2 == "a",]$Y) # condition a,a
shapiro.test(df[df$X1 == "a" & df$X2 == "b",]$Y) # condition a,b
shapiro.test(df[df$X1 == "b" & df$X2 == "a",]$Y) # condition b,a
shapiro.test(df[df$X1 == "b" & df$X2 == "b",]$Y) # condition b,b


##
## Shapiro-Wilk normality test
## (on model residuals)
#
# df has two between-Ss. factors (X1,X2) each w/two levels (a,b) and continuous response (Y)
set.seed(123)
aa = round(rnorm(15, 30.0, 10.0), digits=2)
ab = round(rnorm(15, 40.0, 12.0), digits=2)
ba = round(rnorm(15, 25.0, 7.5), digits=2)
bb = round(rnorm(15, 20.0, 5.0), digits=2)
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
View(df)

msd <- ddply(df, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
)); print(msd)

with(df, interaction.plot(
  X1, 
  X2, 
  Y, 
  ylim=c(min(msd$Mean - msd$SD), max(msd$Mean + msd$SD)), 
  ylab="Y",
  main="Y by X1, X2",
  lty=1, 
  lwd=3, 
  col=c("red","blue")
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

m = aov_ez(dv="Y", between=c("X1","X2"), id="PId", type=3, data=df)
r = residuals(m$lm) # extract residuals
mean(r); sum(r) # both should be ~0
plot(r[1:length(r)], main="Residuals"); abline(h=0) # should look random
qqnorm(r); qqline(r) # Q-Q plot
xax = ceiling(max(abs(min(r)),abs(max(r)))) # balanced histogram
hist(r, main="Histogram of residuals", xlim=c(-xax,+xax), freq=FALSE) 
f = gofTest(r, distribution="norm") # GOF test
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), lty=1, lwd=3, col="blue", add=TRUE)
print.gof(f) # display fit
shapiro.test(r) # Shapiro-Wilk test


##
## Shapiro-Wilk normality test
## (on model residuals)
#
# df has two within-Ss. factors (X1,X2) each w/two levels (a,b) and continuous response (Y)
set.seed(123)
aa = round(rnorm(15, 30.0, 10.0), digits=2)
ab = round(rnorm(15, 40.0, 12.0), digits=2)
ba = round(rnorm(15, 25.0, 7.5), digits=2)
bb = round(rnorm(15, 20.0, 5.0), digits=2)
df = data.frame(
  PId = factor(rep(1:15, times=4)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # renumber row names
View(df)

msd <- ddply(df, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
)); print(msd)

with(df, interaction.plot(
  X1, 
  X2, 
  Y, 
  ylim=c(min(msd$Mean - msd$SD), max(msd$Mean + msd$SD)), 
  ylab="Y",
  main="Y by X1, X2",
  lty=1, 
  lwd=3, 
  col=c("red","blue")
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

m = aov_ez(dv="Y", within=c("X1","X2"), id="PId", type=3, data=df)
r = residuals(m$lm) # extract residuals
mean(r); sum(r) # both should be ~0
plot(r[1:length(r)], main="Residuals"); abline(h=0) # should look random
qqnorm(r); qqline(r) # Q-Q plot
xax = ceiling(max(abs(min(r)),abs(max(r)))) # balanced histogram
hist(r, main="Histogram of residuals", xlim=c(-xax,+xax), freq=FALSE) 
f = gofTest(r, distribution="norm") # GOF test
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), lty=1, lwd=3, col="blue", add=TRUE)
print.gof(f) # display fit
shapiro.test(r) # Shapiro-Wilk test


##
## Shapiro-Wilk test
## (on residuals from linear mixed models)
#
# df has two within-Ss. factors (X1,X2) each w/two levels (a,b) and continuous response (Y)
set.seed(123)
aa = round(rnorm(15, 30.0, 10.0), digits=2)
ab = round(rnorm(15, 40.0, 12.0), digits=2)
ba = round(rnorm(15, 25.0, 7.5), digits=2)
bb = round(rnorm(15, 20.0, 5.0), digits=2)
df = data.frame(
  PId = factor(rep(1:15, times=4)),
  X1 = factor(rep(c("a","b"), each=30)),
  X2 = factor(rep(rep(c("a","b"), each=15), times=2)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # renumber row names
View(df)

msd <- ddply(df, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
)); print(msd)

with(df, interaction.plot(
  X1, 
  X2, 
  Y, 
  ylim=c(min(msd$Mean - msd$SD), max(msd$Mean + msd$SD)), 
  ylab="Y",
  main="Y by X1, X2",
  lty=1, 
  lwd=3, 
  col=c("red","blue")
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

m = lmer(Y ~ X1*X2 + (1|PId), data=df) # make linear mixed model
r = residuals(m) # extract residuals
mean(r); sum(r) # both should be ~0
plot(r[1:length(r)], main="Residuals"); abline(h=0) # should look random
qqnorm(r); qqline(r) # Q-Q plot
xax = ceiling(max(abs(min(r)),abs(max(r)))) # balanced histogram
hist(r, main="Histogram of residuals", xlim=c(-xax,+xax), freq=FALSE) 
f = gofTest(r, distribution="norm") # GOF test
curve(dnorm(x, mean=f$distribution.parameters[1], sd=f$distribution.parameters[2]), lty=1, lwd=3, col="blue", add=TRUE)
print.gof(f) # display fit
shapiro.test(r) # Shapiro-Wilk test



###
#### 2. Homogeneity of variance ####
###

## (Levene's test)
# df has one between-Ss. factor (X1), one within-Ss. factor (X2), and continuous response (Y)
set.seed(123)
aa = round(rnorm(15, 30.0, 10.0), digits=2)
ab = round(rnorm(15, 40.0, 12.0), digits=2)
ba = round(rnorm(15, 25.0, 7.5), digits=2)
bb = round(rnorm(15, 20.0, 5.0), digits=2)
df = data.frame(
  PId = factor(rep(1:30, times=2)),
  X1 = factor(rep(rep(c("a","b"), each=15), times=2)),
  X2 = factor(rep(c("a","b"), each=30)),
  Y = c(aa,ab,ba,bb)
)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # renumber row names
View(df)

msd <- ddply(df, ~ X1 + X2, function(data) c(
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y)
)); print(msd)

with(df, interaction.plot(
  X1, 
  X2, 
  Y, 
  ylim=c(min(msd$Mean - msd$SD), max(msd$Mean + msd$SD)), 
  ylab="Y",
  main="Y by X1, X2",
  lty=1, 
  lwd=3, 
  col=c("red","blue")
))
dx = 0.0035  # nudge
arrows(x0=1-dx, y0=msd[1,]$Mean - msd[1,]$SD, x1=1-dx, y1=msd[1,]$Mean + msd[1,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=1+dx, y0=msd[2,]$Mean - msd[2,]$SD, x1=1+dx, y1=msd[2,]$Mean + msd[2,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")
arrows(x0=2-dx, y0=msd[3,]$Mean - msd[3,]$SD, x1=2-dx, y1=msd[3,]$Mean + msd[3,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="red")
arrows(x0=2+dx, y0=msd[4,]$Mean - msd[4,]$SD, x1=2+dx, y1=msd[4,]$Mean + msd[4,]$SD, angle=90, code=3, lty=1, lwd=3, length=0.2, col="blue")

m = aov_ez(dv="Y", between="X1", within="X2", id="PId", type=3, data=df)
check_homogeneity(m) # Levene's test

# if a violation occurs (p<.05), use a Welch t-test for one factor of two levels...
plot(Y ~ X1, data=df, main="Y by X1")
t.test(Y ~ X1, data=df, var.equal=FALSE)
cohens_d(Y ~ X1, data=df, pooled_sd=FALSE)

# ...or a Welch ANOVA for one factor of >2 levels...
oneway.test(Y ~ X1, data=df, var.equal=FALSE)

# ...or a White-adjusted ANOVA for >1 factor (with within-Ss. factors, use an LMM)
m = lmer(Y ~ X1*X2 + (1|PId), data=df)
Anova(m, type=3, test.statistic="F", white.adjust=TRUE)
eta_squared(m, partial=TRUE)



###
#### 3. Sphericity ####
###

## (Mauchly's test)
# df has one within-Ss. factor (X) w/levels (a,b,c) and continuous response (Y)
set.seed(123)
a = round(rnorm(20, 30.0, 10.0), digits=2)
b = round(rnorm(20, 45.0, 25.0), digits=2)
c = round(rnorm(20, 40.0, 7.50), digits=2)
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
boxplot(Y ~ X, data=df, main="Y by X")

m = aov_ez(dv="Y", within="X", id="PId", type=3, data=df)
summary(m)$sphericity.tests # Mauchly's test of sphericity
check_sphericity(m)         # same

# one-way repeated measures ANOVA
anova(m, correction="none") # use if p≥.05, no violation of sphericity
anova(m, correction="GG")   # use if p<.05, sphericity violation

