###
### Statistical Analysis and Reporting in R
### Jacob O. Wobbrock, Ph.D.
### wobbrock@uw.edu
### The Information School
### University of Washington
### November 14, 2018
### Updated: 11/10/2024
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
a = sample(c("no","yes"), size=60, replace=TRUE, prob=c(0.7, 0.3))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  Y = factor(a)
)
View(df)

ddply(df, .(), function(data) c(
  "Nrows"=nrow(data),
  "no"=sum(data$Y == "no"),
  "yes"=sum(data$Y == "yes")
))
plot( ~ Y, data=df, col=c("pink","lightgreen"), main="Y")

xt = xtabs( ~ Y, data=df) # make counts
View(xt)
binom.test(xt, p=0.5, alternative="two.sided")


##
#### 2. Multinomial test ####
##
# df is a long-format data table w/columns for participant (PId) and N-category outcome (Y)
set.seed(123)
a = sample(c("maybe","no","yes"), size=60, replace=TRUE, prob=c(0.3, 0.2, 0.5))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  Y = factor(a)
)
View(df)

ddply(df, .(), function(data) c(
  "Nrows"=nrow(data),
  "maybe"=sum(data$Y == "maybe"),
  "no"=sum(data$Y == "no"),
  "yes"=sum(data$Y == "yes")
))
plot( ~ Y, data=df, col=c("lightyellow","pink","lightgreen"), main="Y")

xt = xtabs( ~ Y, data=df) # make counts
View(xt)
xmulti(xt, rep(1/length(xt), length(xt)), statName="Prob")

# the following gives the same result
multinomial.test(df$Y)

## Multinomial post hoc test
# For Y's response categories, test each in pairwise fashion against 50/50.
mn = binom.test(c(sum(df$Y == "maybe"), sum(df$Y == "no")), p=1/2)  # maybe vs. no
my = binom.test(c(sum(df$Y == "maybe"), sum(df$Y == "yes")), p=1/2) # maybe vs. yes
ny = binom.test(c(sum(df$Y == "no"), sum(df$Y == "yes")), p=1/2)    # no vs. yes
p.adjust(c(mn$p.value, my$p.value, ny$p.value), method="holm")

# or, equivalently, if xt is a table of counts for each category in Y
multinomial.multcomp(xt, p.method="holm") # same results as above


##
#### 3. One-sample Pearson chi-squared test ####
##
# df is a long-format data table w/columns for participant (PId) and N-category outcome (Y)
set.seed(123)
a = sample(c("maybe","no","yes"), size=60, replace=TRUE, prob=c(0.3, 0.2, 0.5))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  Y = factor(a)
)
View(df)

ddply(df, .(), function(data) c(
  "Nrows"=nrow(data),
  "maybe"=sum(data$Y == "maybe"),
  "no"=sum(data$Y == "no"),
  "yes"=sum(data$Y == "yes")
))
plot( ~ Y, data=df, col=c("lightyellow","pink","lightgreen"), main="Y")

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
m = binom.test(sum(df$Y == "maybe"), nrow(df), p=1/3) # "maybe" rows
n = binom.test(sum(df$Y == "no"), nrow(df), p=1/3)    # "no" rows
y = binom.test(sum(df$Y == "yes"), nrow(df), p=1/3)   # "yes" rows
p.adjust(c(m$p.value, n$p.value, y$p.value), method="holm")




##
#### Two Samples ####
##

##
#### 5. Fisher's exact test ####
##
# df is a long-format data table w/participant (PId), between-Ss. factor (X), and N-category outcome (Y)
set.seed(123)
a = sample(c("maybe","no","yes"), size=30, replace=TRUE, prob=c(0.3, 0.2, 0.5))
b = sample(c("maybe","no","yes"), size=30, replace=TRUE, prob=c(0.7, 0.2, 0.1))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b"), each=30)),
  Y = factor(c(a,b))
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "maybe"=sum(data$Y == "maybe"),
  "no"=sum(data$Y == "no"),
  "yes"=sum(data$Y == "yes")
))
plot( ~ X + Y, data=df, col=c("lightgreen","pink","lightyellow"), main="Y by X")

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
a = sample(c("maybe","no","yes"), size=30, replace=TRUE, prob=c(0.3, 0.2, 0.5))
b = sample(c("maybe","no","yes"), size=30, replace=TRUE, prob=c(0.7, 0.2, 0.1))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b"), each=30)),
  Y = factor(c(a,b))
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "maybe"=sum(data$Y == "maybe"),
  "no"=sum(data$Y == "no"),
  "yes"=sum(data$Y == "yes")
))
plot( ~ X + Y, data=df, col=c("lightgreen","pink","lightyellow"), main="Y by X")

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
a = sample(c("maybe","no","yes"), size=30, replace=TRUE, prob=c(0.3, 0.2, 0.5))
b = sample(c("maybe","no","yes"), size=30, replace=TRUE, prob=c(0.7, 0.2, 0.1))
df = data.frame(
  PId = factor(seq(1, 60, 1)),
  X = factor(rep(c("a","b"), each=30)),
  Y = factor(c(a,b))
)
contrasts(df$X) <- "contr.sum"
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "maybe"=sum(data$Y == "maybe"),
  "no"=sum(data$Y == "no"),
  "yes"=sum(data$Y == "yes")
))
plot( ~ X + Y, data=df, col=c("lightgreen","pink","lightyellow"), main="Y by X")

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
# response categories (maybe, no, yes), test each proportion of Y within each level of 
# X against chance.
am = binom.test(sum(df[df$X == "a",]$Y == "maybe"), nrow(df[df$X == "a",]), p=1/3)
an = binom.test(sum(df[df$X == "a",]$Y == "no"), nrow(df[df$X == "a",]), p=1/3)
ay = binom.test(sum(df[df$X == "a",]$Y == "yes"), nrow(df[df$X == "a",]), p=1/3)
p.adjust(c(am$p.value, an$p.value, ay$p.value), method="holm")

bm = binom.test(sum(df[df$X == "b",]$Y == "maybe"), nrow(df[df$X == "b",]), p=1/3)
bn = binom.test(sum(df[df$X == "b",]$Y == "no"), nrow(df[df$X == "b",]), p=1/3)
by = binom.test(sum(df[df$X == "b",]$Y == "yes"), nrow(df[df$X == "b",]), p=1/3)
p.adjust(c(bm$p.value, bn$p.value, by$p.value), method="holm")




##
#### Dependent Samples ####
##

##
#### 9. Symmetry test for dependent samples ####
##
# df is a long-format data table w/participant (PId), a within-Ss. factor (X), and N-category outcome (Y)
set.seed(123)
fall   = sample(c("chocolate","strawberry","vanilla"), size=15, replace=TRUE, prob=c(0.3, 0.1, 0.6))
winter = sample(c("chocolate","strawberry","vanilla"), size=15, replace=TRUE, prob=c(0.6, 0.3, 0.1))
spring = sample(c("chocolate","strawberry","vanilla"), size=15, replace=TRUE, prob=c(0.1, 0.6, 0.3))
summer = sample(c("chocolate","strawberry","vanilla"), size=15, replace=TRUE, prob=c(0.4, 0.4, 0.2))
df = data.frame(
  PId = factor(rep(1:15, times=4)),
  X = factor(rep(c("fall","winter","spring","summer"), each=15)),
  Y = factor(c(fall, winter, spring, summer))
)
contrasts(df$X) <- "contr.sum"
df <- df[order(df$PId),] # sort by PId
row.names(df) <- 1:nrow(df) # renumber row names
View(df)

ddply(df, ~ X, function(data) c(
  "Nrows"=nrow(data),
  "chocolate"=sum(data$Y == "chocolate"),
  "strawberry"=sum(data$Y == "strawberry"),
  "vanilla"=sum(data$Y == "vanilla")
))
plot( ~ X + Y, data=df, col=c("beige","pink","tan"), main="Ice Cream by Season", xlab="Season")

xt = xtabs( ~ X + Y, data=df)
View(xt)
symmetry_test(Y ~ X | PId, data=df)

## Post hoc sign tests for the "fall" season, with obvious replication to other seasons.
# add new columns to the data table:
df$chose.chocolate.in.fall = ifelse(df$Y == "chocolate" & df$X == "fall", 1, 0)
df$chose.strawberry.in.fall = ifelse(df$Y == "strawberry" & df$X == "fall", 1, 0)
df$chose.vanilla.in.fall = ifelse(df$Y == "vanilla" & df$X == "fall", 1, 0)
View(df)

sum(df$chose.chocolate.in.fall)
sum(df$chose.strawberry.in.fall)
sum(df$chose.vanilla.in.fall)

cs = pvalue(sign_test(chose.chocolate.in.fall ~ chose.strawberry.in.fall, data=df)) # chocolate vs. strawberry
cv = pvalue(sign_test(chose.chocolate.in.fall ~ chose.vanilla.in.fall, data=df))    # chocolate vs. vanilla
sv = pvalue(sign_test(chose.strawberry.in.fall ~ chose.vanilla.in.fall, data=df))   # strawberry vs. vanilla

p.adjust(c(cs, cv, sv), method="holm")


