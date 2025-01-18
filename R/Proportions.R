###
### Statistical Analysis and Reporting in R
### Jacob O. Wobbrock, Ph.D.
### wobbrock@uw.edu
### The Information School
### University of Washington
### November 14, 2018
### Updated: 1/17/2025
###

###
### Proportions.R
### (Tests of proportion and association)
###

library(plyr) # for ddply
library(XNomial) # for xmulti
library(RVAideMemoire) # for multinomial.test, multinomial.multcomp, chisq.multcomp, fisher.multcomp, G.test, G.multcomp
library(coin) # for symmetry_test, sign_test


## 
#### One Sample ####
##

##
#### 1. Binomial test ####
##
# df is a long-format data table w/columns for participant (PId) and 2-category outcome (Y)
set.seed(123)
a = sample(c("yes","no"), size=60, replace=TRUE, prob=c(0.7, 0.3))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  Y = factor(a, levels=c("yes","no"))
)
View(df)

ddply(df, .(), function(data) c(
  "Nrows"=nrow(data),
  "yes"=sum(data$Y == "yes"),
  "no"=sum(data$Y == "no")
))
plot( ~ Y, data=df, col=c("lightgreen","pink"), main="Y")

xt = xtabs( ~ Y, data=df) # make counts
View(xt)
binom.test(xt, p=0.5, alternative="two.sided")


##
#### 2. Multinomial test ####
##
# df is a long-format data table w/columns for participant (PId) and N-category outcome (Y)
set.seed(123)
a = sample(c("yes","no","maybe"), size=60, replace=TRUE, prob=c(0.3, 0.2, 0.5))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  Y = factor(a, levels=c("yes","no","maybe"))
)
View(df)

ddply(df, .(), function(data) c(
  "Nrows"=nrow(data),
  "yes"=sum(data$Y == "yes"),
  "no"=sum(data$Y == "no"),
  "maybe"=sum(data$Y == "maybe")
))
plot( ~ Y, data=df, col=c("lightgreen","pink","lightyellow"), main="Y")

xt = xtabs( ~ Y, data=df) # make counts
View(xt)
xmulti(xt, rep(1/length(xt), length(xt)), statName="Prob")

# the following gives the same result
multinomial.test(df$Y)

## Multinomial post hoc test
# For Y's response categories, test each in pairwise fashion against 50/50.
yn = binom.test(c(sum(df$Y == "yes"), sum(df$Y == "no")), p=1/2)    # yes vs. no
ym = binom.test(c(sum(df$Y == "yes"), sum(df$Y == "maybe")), p=1/2) # yes vs. maybe
nm = binom.test(c(sum(df$Y == "no"), sum(df$Y == "maybe")), p=1/2)  # no vs. maybe
p.adjust(c(yn$p.value, ym$p.value, nm$p.value), method="holm")

# or, equivalently, if xt is a table of counts for each category in Y
multinomial.multcomp(xt, p.method="holm") # same results as above


##
#### 3. One-sample Pearson chi-squared test ####
##
# df is a long-format data table w/columns for participant (PId) and N-category outcome (Y)
set.seed(123)
a = sample(c("yes","no","maybe"), size=60, replace=TRUE, prob=c(0.3, 0.2, 0.5))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  Y = factor(a, levels=c("yes","no","maybe"))
)
View(df)

ddply(df, .(), function(data) c(
  "Nrows"=nrow(data),
  "yes"=sum(data$Y == "yes"),
  "no"=sum(data$Y == "no"),
  "maybe"=sum(data$Y == "maybe")
))
plot( ~ Y, data=df, col=c("lightgreen","pink","lightyellow"), main="Y")

xt = xtabs( ~ Y, data=df) # make counts
View(xt)
chisq.test(xt)

## Chi-squared post hoc test
# xt is a table of counts for each category of Y
chisq.multcomp(xt, p.method="holm") # xt shows levels
# for the chi-squared values, use qchisq(1-p, df=1), where p is the pairwise p-value:
qchisq(1 - .011, df=1) # 6.465


##
#### 4. One-sample binomial tests against chance ####
##
# A different kind of post hoc test for one sample. For Y's response categories, 
# test each proportion against chance.
y = binom.test(sum(df$Y == "yes"), nrow(df), p=1/3)   # "yes" rows
n = binom.test(sum(df$Y == "no"), nrow(df), p=1/3)    # "no" rows
m = binom.test(sum(df$Y == "maybe"), nrow(df), p=1/3) # "maybe" rows
p.adjust(c(y$p.value, n$p.value, m$p.value), method="holm")




##
#### Two Samples ####
##

##
#### 5. Fisher's exact test ####
##
# df is a long-format data table w/participant (PId), between-Ss. factor (X), and N-category outcome (Y)
set.seed(123)
a = sample(c("yes","no","maybe"), size=30, replace=TRUE, prob=c(0.3, 0.2, 0.5))
b = sample(c("yes","no","maybe"), size=30, replace=TRUE, prob=c(0.7, 0.2, 0.1))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b"), each=30)),
  Y = factor(c(a,b), levels=c("yes","no","maybe"))
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "yes"=sum(data$Y == "yes"),
  "no"=sum(data$Y == "no"),
  "maybe"=sum(data$Y == "maybe")
))
plot( ~ X + Y, data=df, col=c("lightyellow","pink","lightgreen"), main="Y by X")

xt = xtabs( ~ X + Y, data=df) # make counts
View(xt)
fisher.test(xt)

## Fisher's post hoc test
# xt is an m × n crosstabs with categories X and Y
fisher.multcomp(xt, p.method="holm")


##
#### 6. G-test ####
##
# df is a long-format data table w/participant (PId), between-Ss. factor (X), and N-category outcome (Y)
set.seed(123)
a = sample(c("yes","no","maybe"), size=30, replace=TRUE, prob=c(0.3, 0.2, 0.5))
b = sample(c("yes","no","maybe"), size=30, replace=TRUE, prob=c(0.7, 0.2, 0.1))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b"), each=30)),
  Y = factor(c(a,b), levels=c("yes","no","maybe"))
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "yes"=sum(data$Y == "yes"),
  "no"=sum(data$Y == "no"),
  "maybe"=sum(data$Y == "maybe")
))
plot( ~ X + Y, data=df, col=c("lightyellow","pink","lightgreen"), main="Y by X")

xt = xtabs( ~ X + Y, data=df) # make counts
View(xt)
G.test(xt)

## G post hoc test
# xt is an m × n crosstabs with categories X and Y
G.multcomp(xt, p.method="holm") # xt shows levels


##
#### 7. Two-sample Pearson chi-squared test ####
##
# df is a long-format data table w/participant (PId), between-Ss. factor (X), and N-category outcome (Y)
set.seed(123)
a = sample(c("yes","no","maybe"), size=30, replace=TRUE, prob=c(0.3, 0.2, 0.5))
b = sample(c("yes","no","maybe"), size=30, replace=TRUE, prob=c(0.7, 0.2, 0.1))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b"), each=30)),
  Y = factor(c(a,b), levels=c("yes","no","maybe"))
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "yes"=sum(data$Y == "yes"),
  "no"=sum(data$Y == "no"),
  "maybe"=sum(data$Y == "maybe")
))
plot( ~ X + Y, data=df, col=c("lightyellow","pink","lightgreen"), main="Y by X")

xt = xtabs( ~ X + Y, data=df) # make counts
View(xt)
chisq.test(xt)

## Chi-Squared post hoc test
# xt is an m × n crosstabs with categories X and Y
chisq.multcomp(xt, p.method="holm") # xt shows levels
# for the Chi-Squared values, use qchisq(1-p, df=1), where p is the pairwise p-value:
qchisq(1 - 0.00067, df=1) # 11.571


##
#### 8. Two-sample binomial tests against chance ####
##
# A different kind of post hoc test for two samples. For X's categories (a,b) and Y's 
# response categories (yes, no, maybe), test each proportion of Y within each level of 
# X against chance.
ay = binom.test(sum(df[df$X == "a",]$Y == "yes"), nrow(df[df$X == "a",]), p=1/3)
an = binom.test(sum(df[df$X == "a",]$Y == "no"), nrow(df[df$X == "a",]), p=1/3)
am = binom.test(sum(df[df$X == "a",]$Y == "maybe"), nrow(df[df$X == "a",]), p=1/3)
p.adjust(c(ay$p.value, an$p.value, am$p.value), method="holm")

by = binom.test(sum(df[df$X == "b",]$Y == "yes"), nrow(df[df$X == "b",]), p=1/3)
bn = binom.test(sum(df[df$X == "b",]$Y == "no"), nrow(df[df$X == "b",]), p=1/3)
bm = binom.test(sum(df[df$X == "b",]$Y == "maybe"), nrow(df[df$X == "b",]), p=1/3)
p.adjust(c(by$p.value, bn$p.value, bm$p.value), method="holm")




##
#### Dependent Samples ####
##

##
#### 9. Symmetry test for dependent samples ####
##
# df is a long-format data table w/participant (PId), a within-Ss. factor (X), and N-category outcome (Y)
set.seed(123)
fall   = sample(c("vanilla","chocolate","strawberry"), size=15, replace=TRUE, prob=c(0.6, 0.3, 0.1))
winter = sample(c("vanilla","chocolate","strawberry"), size=15, replace=TRUE, prob=c(0.1, 0.6, 0.3))
spring = sample(c("vanilla","chocolate","strawberry"), size=15, replace=TRUE, prob=c(0.3, 0.1, 0.6))
summer = sample(c("vanilla","chocolate","strawberry"), size=15, replace=TRUE, prob=c(0.2, 0.4, 0.4))
df = data.frame(
  PId = factor(rep(1:15, times=4)),
  X = factor(rep(c("fall","winter","spring","summer"), each=15), levels=c("fall","winter","spring","summer")),
  Y = factor(c(fall, winter, spring, summer), levels=c("vanilla","chocolate","strawberry"))
)
contrasts(df$X) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # renumber row names
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "vanilla"=sum(data$Y == "vanilla"),
  "chocolate"=sum(data$Y == "chocolate"),
  "strawberry"=sum(data$Y == "strawberry")
))
plot( ~ X + Y, data=df, col=c("pink","tan","beige"), main="Ice Cream by Season", xlab="Season")

xt = xtabs( ~ X + Y, data=df)
View(xt)
symmetry_test(Y ~ X | PId, data=df)

## Post hoc sign tests for the "vanilla" flavor, with obvious replication to other flavors.
# Add new columns to the data table:
df$chose.vanilla.in.fall = ifelse(df$Y == "vanilla" & df$X == "fall", 1, 0)
df$chose.vanilla.in.winter = ifelse(df$Y == "vanilla" & df$X == "winter", 1, 0)
df$chose.vanilla.in.spring = ifelse(df$Y == "vanilla" & df$X == "spring", 1, 0)
df$chose.vanilla.in.summer = ifelse(df$Y == "vanilla" & df$X == "summer", 1, 0)
View(df)

sum(df$chose.vanilla.in.fall)   # 9
sum(df$chose.vanilla.in.winter) # 2
sum(df$chose.vanilla.in.spring) # 3
sum(df$chose.vanilla.in.summer) # 2

fa.wi = pvalue(sign_test(chose.vanilla.in.fall ~ chose.vanilla.in.winter, data=df)) # fall vs. winter
fa.sp = pvalue(sign_test(chose.vanilla.in.fall ~ chose.vanilla.in.spring, data=df)) # fall vs. spring
fa.su = pvalue(sign_test(chose.vanilla.in.fall ~ chose.vanilla.in.summer, data=df)) # fall vs. summer
wi.sp = pvalue(sign_test(chose.vanilla.in.winter ~ chose.vanilla.in.spring, data=df)) # winter vs. spring
wi.su = pvalue(sign_test(chose.vanilla.in.winter ~ chose.vanilla.in.summer, data=df)) # winter vs. summer
sp.su = pvalue(sign_test(chose.vanilla.in.spring ~ chose.vanilla.in.summer, data=df)) # spring vs. summer

p.adjust(c(fa.wi, fa.sp, fa.su, wi.sp, wi.su, sp.su), method="holm")


