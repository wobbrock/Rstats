###
### Statistical Analysis and Reporting in R
### Jacob O. Wobbrock, Ph.D.
### wobbrock@uw.edu
### The Information School
### University of Washington
### March 12, 2019
### Updated: 11/10/2024
###

###
### Assumptions.R
### (Tests of ANOVA assumptions: Normality, homoscedasticity, sphericity)
###

library(plyr) # for ddply
library(afex) # for aov_ez
library(performance) # for check_normality, check_homogeneity, check_sphericity
library(lme4) # for lmer
library(lmerTest)
library(car) # for Anova


###
#### 1. Normality ####
###

##
## Shapiro-Wilk normality tests
## (on the response within each condition)
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

m = aov_ez(dv="Y", between=c("X1","X2"), id="PId", type=3, data=df)

r = residuals(m$lm) # extract residuals
mean(r); sum(r) # both should be ~0
plot(r[1:length(r)], main="Residual plot"); abline(h=0) # should look random
hist(r, main="Histogram of residuals") # should look normal
qqnorm(r); qqline(r) # Q-Q plot

shapiro.test(r) # Shapiro-Wilk test
print(check_normality(m)) # same


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

m = aov_ez(dv="Y", within=c("X1","X2"), id="PId", type=3, data=df)

r = residuals(m$lm) # extract residuals
mean(r); sum(r) # both should be ~0
plot(r[1:length(r)], main="Residual plot"); abline(h=0) # should look random
hist(r, main="Histogram of residuals") # should look normal
qqnorm(r); qqline(r) # Q-Q plot

shapiro.test(r) # Shapiro-Wilk test
print(check_normality(m)) # same


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

m = lmer(Y ~ X1*X2 + (1|PId), data=df) # make linear mixed model

r = residuals(m) # extract model residuals
mean(r); sum(r) # both should be ~0
plot(r[1:length(r)], main="Residual plot"); abline(h=0) # should look random
hist(r, main="Histogram of residuals") # should look normal
qqnorm(r); qqline(r) # Q-Q plot

shapiro.test(r) # Shapiro-Wilk test
print(check_normality(m)) # same



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

m = aov_ez(dv="Y", between="X1", within="X2", id="PId", type=3, data=df)
print(check_homogeneity(m)) # Levene's test

# if a violation occurs (p<.05), use a Welch t-test for one factor of two levels...
plot(Y ~ X1, data=df, main="Y by X1")
t.test(Y ~ X1, data=df, var.equal=FALSE)

# ...or a White-adjusted ANOVA for >1 factor or >2 levels
Anova(m$lm, type=3, white.adjust=TRUE) # shows between-Ss. factors only



###
#### 3. Sphericity ####
###

## (Mauchly's test)
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

m = aov_ez(dv="Y", between="X1", within="X2", id="PId", type=3, data=df)
print(check_sphericity(m))

# one-way repeated measures ANOVA
anova(m, correction="none") # use if p≥.05, no violation of sphericity
anova(m, correction="GG")   # use if p<.05, sphericity violation



