###
### #Rstats
###
### Statistical Inference in R
### Jacob O. Wobbrock, Ph.D.
### wobbrock@uw.edu
### The Information School
### University of Washington
### November 26, 2018
### Updated: 2/21/2026
###

###
### Nonparametric.R
### (Nonparametric analyses of differences)
###

library(plyr)        # for ddply, mutate
library(dplyr)       # for %>%
library(performance) # for check_*
library(lme4)        # for lmer
library(lmerTest)    # for lmer
library(rcompanion)  # for wilcoxonZ
library(reshape2)    # for dcast
library(ARTool)      # for art, art.con
library(coin)        # for median_test, 
                     #     sign_test, 
                     #     wilcox_test, 
                     #     wilcoxsign_test, 
                     #     kruskal_test, 
                     #     friedman_test


##
#### One Factor, Two Levels ####
##

##
#### 1. Median test ####
#

# df has one between-Ss. factor (X) w/levels (a,b) and a (1,0) response
set.seed(123)
a = sample(c(1,0), 30, replace=TRUE, prob=c(0.7, 0.3))
b = sample(c(1,0), 30, replace=TRUE, prob=c(0.3, 0.7))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b"), each=30)),
  Y = c(a,b)
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "1"=sum(data$Y == 1),
  "0"=sum(data$Y == 0)
))

barplot(c(
  sum(df[df$X == "a",]$Y), 
  sum(df[df$X == "b",]$Y)),
  names.arg=c("a","b"),
  main="Sum Y by X",
  xlab="X",
  ylab="Sum",
  col=c("pink","lightblue")
)

m0 = lm(Y ~ X, data=df)
check_normality(m0)
check_homogeneity(m0)

median_test(Y ~ X, data=df)



##
#### 2. Sign test ####
#

# df has one within-Ss. factor (X) w/levels (a,b) and a (1,0) response
set.seed(123)
a = sample(c(1,0), 30, replace=TRUE, prob=c(0.7, 0.3))
b = sample(c(1,0), 30, replace=TRUE, prob=c(0.3, 0.7))
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
  "1"=sum(data$Y == 1),
  "0"=sum(data$Y == 0)
))

barplot(c(
  sum(df[df$X == "a",]$Y), 
  sum(df[df$X == "b",]$Y)),
  names.arg=c("a","b"),
  main="Sum of Y by X",
  xlab="X",
  ylab="Sum",
  col=c("pink","lightblue")
)

m0 = lmer(Y ~ X + (1|PId), data=df)
check_normality(m0)

sign_test(Y ~ X | PId, data=df)



##
#### 3. Mann-Whitney U test ####
#

# df has one between-Ss. factor (X) w/levels (a,b) and continuous response (Y)
set.seed(123)
a = round(rnorm(30, 30.0, 12.0), digits=2)
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

boxplot(
  Y ~ X, 
  main="Y by X", 
  col=c("pink","lightblue"),
  data=df
)

m0 = lm(Y ~ X, data=df)
check_normality(m0)
check_homogeneity(m0)

wilcox_test(Y ~ X, data=df) # Mann-Whitney U test



##
#### 4. Wilcoxon signed-rank test ####
#

# df one within-Ss. factor (X) w/levels (a,b) and continuous response (Y)
set.seed(123)
a = round(rnorm(30, 30.0, 12.0), digits=2)
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

boxplot(
  Y ~ X, 
  main="Y by X", 
  col=c("pink","lightblue"),
  data=df
)

m0 = lmer(Y ~ X + (1|PId), data=df)
check_normality(m0)

wilcoxsign_test(Y ~ X | PId, data=df, distribution="exact") # Wilcoxon signed-rank test



##
#### One Factor, Multiple Levels ####
## 

##
#### 5. Kruskal-Wallis test ####
#

# df has one between-Ss. factor (X) w/levels (a,b,c) and continuous response (Y)
set.seed(123)
a = round(rnorm(20, 30.0, 12.0), digits=2)
b = round(rnorm(20, 45.0, 15.0), digits=2)
c = round(rnorm(20, 40.0, 14.0), digits=2)
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

boxplot(
  Y ~ X, 
  main="Y by X", 
  col=c("pink","lightblue","lightgreen"),
  data=df
)

m0 = lm(Y ~ X, data=df)
check_normality(m0)
check_homogeneity(m0)

kruskal_test(Y ~ X, data=df, distribution="asymptotic") # Kruskal-Wallis test

## Post hoc Mann-Whitney U tests
ab = wilcox.test(df[df$X == "a",]$Y, df[df$X == "b",]$Y, exact=FALSE) # a vs. b
ac = wilcox.test(df[df$X == "a",]$Y, df[df$X == "c",]$Y, exact=FALSE) # a vs. c
bc = wilcox.test(df[df$X == "b",]$Y, df[df$X == "c",]$Y, exact=FALSE) # b vs. c
p.adjust(c(ab$p.value, ac$p.value, bc$p.value), method="holm") # p-values

wilcoxonZ(df[df$X == "a",]$Y, df[df$X == "b",]$Y) # Z-score
wilcoxonZ(df[df$X == "a",]$Y, df[df$X == "c",]$Y) # Z-score
wilcoxonZ(df[df$X == "b",]$Y, df[df$X == "c",]$Y) # Z-score



##
#### 6. Friedman test ####
#

# df has one within-Ss. factor (X) w/levels (a,b,c) and continuous response (Y)
set.seed(123)
a = round(rnorm(20, 30.0, 12.0), digits=2)
b = round(rnorm(20, 45.0, 15.0), digits=2)
c = round(rnorm(20, 40.0, 14.0), digits=2)
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

boxplot(
  Y ~ X, 
  main="Y by X", 
  col=c("pink","lightblue","lightgreen"),
  data=df
)

m0 = lmer(Y ~ X + (1|PId), data=df)
check_normality(m0)

friedman_test(Y ~ X | PId, data=df, distribution="asymptotic")

## Post hoc Wilcoxon signed-rank tests
df2 <- dcast(df, PId ~ X, value.var="Y") # make wide-format table
ab = wilcox.test(df2$a, df2$b, paired=TRUE, exact=FALSE) # a vs. b
ac = wilcox.test(df2$a, df2$c, paired=TRUE, exact=FALSE) # a vs. c
bc = wilcox.test(df2$b, df2$c, paired=TRUE, exact=FALSE) # b vs. c
p.adjust(c(ab$p.value, ac$p.value, bc$p.value), method="holm") # p-values

wilcoxonZ(df2$a, df2$b, paired=TRUE) # Z-score
wilcoxonZ(df2$a, df2$c, paired=TRUE) # Z-score
wilcoxonZ(df2$b, df2$c, paired=TRUE) # Z-score



##
#### Multiple Factors ####
## 

##
#### 7. Aligned Rank Transform (between-Ss.) ####
#

# df has two between-Ss. factors (X1,X2) each w/levels (a,b) and continuous response (Y)
set.seed(123)
aa = round(runif(15, 20.0, 40.0), digits=2)
ab = round(runif(15, 25.0, 45.0), digits=2)
ba = round(runif(15, 15.0, 30.0), digits=2)
bb = round(runif(15, 20.0, 45.0), digits=2)
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
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
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

m0 = lm(Y ~ X1*X2, data=df)
check_normality(m0)
check_homogeneity(m0)

m = art(Y ~ X1*X2, data=df)
anova(m)

## Post hoc ART-C contrast tests
art.con(m, ~ X1*X2, adjust="holm") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  plyr::mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE, 
                             cutpoints = c(0, .001, .01, .05, .10, 1), 
                             symbols = c("***", "**", "*", ".", " ")))

## Interaction contrasts (between-Ss.)
# Interaction contrasts (Marascuilo & Levin 1970, Boik 1979) are a different type of contrast test 
# that work with ART. In the output, A-B : C-D is interpreted as answering whether the A vs. B 
# difference in condition C is significantly different than the A vs. B difference in condition 
# D. It is a "difference of differences" contrast.
art.con(m, ~ X1*X2, adjust="holm", interaction=TRUE)



##
#### 8. Aligned Rank Transform (within-Ss.) ####
#

# df has two within-Ss. factors (X1,X2) each w/levels (a,b) and continuous response (Y)
set.seed(123)
aa = round(runif(15, 20.0, 40.0), digits=2)
ab = round(runif(15, 25.0, 45.0), digits=2)
ba = round(runif(15, 15.0, 30.0), digits=2)
bb = round(runif(15, 20.0, 45.0), digits=2)
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
  "Nrows"=nrow(data),
  "Min"=min(data$Y),
  "Mean"=mean(data$Y), 
  "SD"=sd(data$Y),
  "Median"=median(data$Y),
  "IQR"=IQR(data$Y),
  "Max"=max(data$Y)
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

m0 = lmer(Y ~ X1*X2 + (1|PId), data=df)
check_normality(m0)

m = art(Y ~ X1*X2 + (1|PId), data=df) # PId is a random factor
anova(m)

## Post hoc ART-C contrast tests
art.con(m, ~ X1*X2, adjust="holm") %>%  # run ART-C for X1×X2
  summary() %>%  # add significance stars to the output
  plyr::mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                             cutpoints = c(0, .001, .01, .05, .10, 1),
                             symbols = c("***", "**", "*", ".", " ")))

## Interaction contrasts (within-Ss.)
# Interaction contrasts (Marascuilo & Levin 1970, Boik 1979) are a different type of contrast test 
# that work with ART. In the output, A-B : C-D is interpreted as answering whether the A vs. B 
# difference in condition C is significantly different than the A vs. B difference in condition 
# D. It is a "difference of differences" contrast.
art.con(m, ~ X1*X2, adjust="holm", interaction=TRUE)


