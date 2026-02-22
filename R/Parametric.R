###
### #Rstats
###
### Statistical Inference in R
### Jacob O. Wobbrock, Ph.D.
### wobbrock@uw.edu
### The Information School
### University of Washington
### November 13, 2019
### Updated: 2/21/2026
###

###
### Parametric.R
### (Parametric analyses of variance)
###

library(plyr)        # for ddply
library(car)         # for leveneTest
library(reshape2)    # for dcast
library(afex)        # for aov_ez
library(performance) # for check_*
library(lme4)        # for lmer
library(lmerTest)    # for lmer
library(effectsize)  # for cohens_d, eta_squared
library(emmeans)     # for emmeans, eff_size


###
#### One Factor ####
###

##
#### 1. Independent-samples t-test ####
##

# df has one between-Ss. factor (X) w/levels (a,b) and continuous response (Y)
set.seed(123)
a = round(rnorm(30, 30.0, 15.0), digits=2)
b = round(rnorm(30, 45.0, 15.0), digits=2)
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
plot(Y ~ X, main="Y by X", data=df)

leveneTest(Y ~ X, data=df, center=mean) # check homogeneity of variance

t.test(Y ~ X, data=df, var.equal=TRUE)  # if p≥.05, no violation of homogeneity
cohens_d(Y ~ X, data=df, pooled_sd=TRUE)

t.test(Y ~ X, data=df, var.equal=FALSE) # if p<.05, Welch t-test
cohens_d(Y ~ X, data=df, pooled_sd=FALSE)



##
#### 2. Paired-samples t-test ####
##

# df has one within-Ss. factor (X) w/levels (a,b) and continuous response (Y)
set.seed(123)
a = round(rnorm(30, 30.0, 15.0), digits=2)
b = round(rnorm(30, 45.0, 15.0), digits=2)
df = data.frame(
  PId = factor(rep(1:30, times=2)),
  X = factor(rep(c("a","b"), each=30)),
  Y = c(a,b)
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
plot(Y ~ X, main="Y by X", data=df)

df2 <- dcast(df, PId ~ X, value.var="Y") # make wide-format table
t.test(df2$a, df2$b, paired=TRUE)        # neither homogeneity nor sphericity applies to a paired t-test
cohens_d(df2$a, df2$b, paired=TRUE)



##
#### 3. One-way ANOVA ####
##

# df has one between-Ss. factor (X) w/levels (a,b,c) and continuous response (Y)
set.seed(123)
a = round(rnorm(20, 30.0, 15.0), digits=2)
b = round(rnorm(20, 45.0, 15.0), digits=2)
c = round(rnorm(20, 40.0, 15.0), digits=2)
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
plot(Y ~ X, data=df, main="Y by X")

m = aov_ez(dv="Y", between="X", id="PId", type=3, data=df)
leveneTest(Y ~ X, data=df, center=mean) # Levene's test
check_homogeneity(m) # same

anova(m) # use if p≥.05, no violation of homoscedasticity, else use...

oneway.test(Y ~ X, data=df, var.equal=FALSE) # Welch ANOVA

Anova(m$lm, type=3, white.adjust=TRUE)       # White-adjusted ANOVA
eta_squared(m$lm, generalized=TRUE)

## post hoc pairwise comparisons
emm = emmeans(m, pairwise ~ X, adjust="holm")
emm$contrasts
eff_size(emm$emmeans, sigma=sigma(m$lm), edf=df.residual(m$lm))



##
#### 4. One-way repeated measures ANOVA ####
##

# df has one within-Ss. factor (X) w/levels (a,b,c) and continuous response (Y)
set.seed(123)
a = round(rnorm(20, 30.0, 15.0), digits=2)
b = round(rnorm(20, 45.0, 15.0), digits=2)
c = round(rnorm(20, 40.0, 15.0), digits=2)
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
plot(Y ~ X, data=df, main="Y by X")

m = aov_ez(dv="Y", within="X", id="PId", type=3, data=df)
summary(m)$sphericity.tests # Mauchly's test of sphericity
check_sphericity(m)         # same

anova(m, correction="none") # use if p≥.05, no violation of sphericity
anova(m, correction="GG")   # use if p<.05, sphericity violation

## post hoc pairwise comparisons
emm = emmeans(m, pairwise ~ X, adjust="holm")
emm$contrasts
sig = sqrt(anova(m, correction="none")["X","MSE"]) # pooled residual SD
eff_size(emm$emmeans, sigma=sig, edf=df.residual(m$lm))




###
#### Multiple Between-Ss. Factors ####
###

##
#### 5. Factorial ANOVA ####
##

# df has two between-Ss. factors (X1,X2) each w/levels (a,b) and continuous response (Y)
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
leveneTest(Y ~ X1*X2, data=df, center=mean) # Levene's test
check_homogeneity(m) # same

anova(m) # use if p≥.05, no violation of homoscedasticity, else use...

Anova(m$lm, type=3, white.adjust=TRUE) # White-adjusted ANOVA
eta_squared(m$lm, generalized=TRUE)

## post hoc pairwise comparisons
emm = emmeans(m, pairwise ~ X1*X2, adjust="holm")
emm$contrasts
eff_size(emm$emmeans, sigma=sigma(m$lm), edf=df.residual(m$lm))



##
#### 6. Linear Model (LM) ####
##

# df has two between-Ss. factors (X1,X2) each w/levels (a,b) and continuous response (Y)
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

m = lm(Y ~ X1*X2, data=df)  # regression model
m = aov(Y ~ X1*X2, data=df) # ANOVA model
leveneTest(Y ~ X1*X2, data=df, center=mean) # Levene's test
check_homogeneity(m)

anova(m) # use if p≥.05, no violation of homogeneity, else use...
eta_squared(m, generalized=TRUE)

Anova(m, type=3, white.adjust=TRUE) # White-adjusted ANOVA
eta_squared(m, generalized=TRUE)

## post hoc pairwise comparisons
emm = emmeans(m, pairwise ~ X1*X2, adjust="holm")
emm$contrasts
eff_size(emm$emmeans, sigma=sigma(m), edf=df.residual(m))




###
#### Multiple Within-Ss. Factors ####
###

##
#### 7. Factorial repeated measures ANOVA ####
##

# df has two within-Ss. factors (X1,X2) each w/levels (a,b) and continuous response (Y)
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
row.names(df) <- 1:nrow(df) # restore row numbers
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

m = aov_ez(dv="Y", within=c("X1","X2"), id="PId", type=3, data=df) # fit model
summary(m)$sphericity.tests # Mauchly's test of sphericity
check_sphericity(m) # same

anova(m, correction="none") # use if p≥.05, no violation of sphericity
anova(m, correction="GG")   # Greenhouse-Geisser correction

## post hoc pairwise comparisons
emm = emmeans(m, pairwise ~ X1*X2, adjust="holm")
emm$contrasts
sig = sqrt(anova(m, correction="none")["X1:X2","MSE"])
eff_size(emm$emmeans, sigma=sig, edf=df.residual(m$lm))



##
#### 8. Linear Mixed Model (LMM) ####
##

# df has two within-Ss. factors (X1,X2) each w/levels (a,b), and continuous response (Y)
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
row.names(df) <- 1:nrow(df) # restore row numbers
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

m = lmer(Y ~ X1*X2 + (1|PId), data=df) # sphericity is not applicable to LMMs
Anova(m, type=3, test.statistic="F")
eta_squared(m, partial=TRUE)
  
## post hoc pairwise comparisons
emm = emmeans(m, pairwise ~ X1*X2, adjust="holm")
emm$contrasts
eff_size(emm$emmeans, sigma=sigma(m), edf=df.residual(m))

