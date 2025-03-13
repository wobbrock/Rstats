###
### Statistical Analysis and Reporting in R
### Jacob O. Wobbrock, Ph.D.
### wobbrock@uw.edu
### The Information School
### University of Washington
### November 14, 2018
### Updated: 3/12/2025
###

###
### Proportions.R
### (Tests of proportion and association)
###

library(plyr) # for ddply
library(XNomial) # for xmulti
library(chisq.posthoc.test) # for chisq.posthoc.test
library(coin) # for symmetry_test, sign_test, pvalue
library(RVAideMemoire) # for multinomial.test, 
                       #     multinomial.multcomp, 
                       #     chisq.multcomp, 
                       #     fisher.multcomp, 
                       #     G.test, 
                       #     G.multcomp


## 
#### One Sample - Tests of Proportions ####
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
xmulti(xt, rep(1/length(xt), length(xt)), statName="Prob") # omnibus test

# or, equivalently
multinomial.test(df$Y)

## Post hoc tests
# For Y's response categories, test each in pairwise fashion against chance
yn = binom.test(c(xt[1], xt[2]), p=1/2) # yes vs. no
ym = binom.test(c(xt[1], xt[3]), p=1/2) # yes vs. maybe
nm = binom.test(c(xt[2], xt[3]), p=1/2) # no vs. maybe
p.adjust(c(yn$p.value, ym$p.value, nm$p.value), method="holm")

# or, equivalently
multinomial.multcomp(xt, p.method="holm") # same results as above

# For Y's response categories, test each against a chance proportion of the total.
y = binom.test(xt[1], nrow(df), p=1/length(xt)) # yes
n = binom.test(xt[2], nrow(df), p=1/length(xt)) # no
m = binom.test(xt[3], nrow(df), p=1/length(xt)) # maybe
p.adjust(c(y$p.value, n$p.value, m$p.value), method="holm")



##
#### 3. One-sample chi-squared test ####
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
chisq.test(xt) # omnibus test

## Post hoc tests
chisq.multcomp(xt, p.method="holm") # xt shows levels
# to get the chi-squared statistics, use qchisq(1-p, df=1),
# where p is the uncorrected (p.method="none“) pairwise p-value:
qchisq(1 - 0.0038, df=1) # 8.376996



##
#### Two Samples - Tests of Association ####
##

##
#### 4. Fisher's exact test ####
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
mosaicplot( ~ X + Y, data=df, col=c("lightgreen","pink","lightyellow"), main="Y by X")

xt = xtabs( ~ X + Y, data=df) # make counts
View(xt)
fisher.test(xt) # omnibus test

## Post hoc tests of 2x2 table subsets
yn = fisher.test(xt[,c(1,2)]) # yes vs. no
ym = fisher.test(xt[,c(1,3)]) # yes vs. maybe
nm = fisher.test(xt[,c(2,3)]) # no vs. maybe
p.adjust(c(yn$p.value, ym$p.value, nm$p.value), method="holm")

# or, equivalently
fisher.multcomp(xt, p.method="holm")

# or, test each column against chance
y = binom.test(xt[,1]) # yes
n = binom.test(xt[,2]) # no
m = binom.test(xt[,3]) # maybe
p.adjust(c(y$p.value, n$p.value, m$p.value), method="holm")

# or, test each row against chance
a = xmulti(xt[1,], rep(1/length(xt[1,]), length(xt[1,])), statName="Prob") # a
b = xmulti(xt[2,], rep(1/length(xt[2,]), length(xt[2,])), statName="Prob") # b
p.adjust(c(a$pProb, b$pProb), method="holm")



##
#### 5. G-test ####
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
mosaicplot( ~ X + Y, data=df, col=c("lightgreen","pink","lightyellow"), main="Y by X")

xt = xtabs( ~ X + Y, data=df) # make counts
View(xt)
G.test(xt) # omnibus test

## Post hoc tests of 2x2 table subsets
yn = G.test(xt[,c(1,2)]) # yes vs. no
ym = G.test(xt[,c(1,3)]) # yes vs. maybe
nm = G.test(xt[,c(2,3)]) # no vs. maybe
p.adjust(c(yn$p.value, ym$p.value, nm$p.value), method="holm")

# or, do all cellwise comparisons
G.multcomp(xt, p.method="holm") # xt shows levels

# or, test each column against chance
y = G.test(xt[,1]) # yes
n = G.test(xt[,2]) # no
m = G.test(xt[,3]) # maybe
p.adjust(c(y$p.value, n$p.value, m$p.value), method="holm")

# or, test each row against chance
a = G.test(xt[1,]) # a
b = G.test(xt[2,]) # b
p.adjust(c(a$p.value, b$p.value), method="holm")

# or, test each column against expected frequencies
ex = G.test(xt)$expected[,1] # expected 'yes'
y = G.test(xt[,1], p=ex/sum(ex))
ex = G.test(xt)$expected[,2] # expected 'no'
n = G.test(xt[,2], p=ex/sum(ex))
ex = G.test(xt)$expected[,3] # expected 'maybe'
m = G.test(xt[,3], p=ex/sum(ex))
p.adjust(c(y$p.value, n$p.value, m$p.value), method="holm")

# or, test each row against expected frequencies
ex = G.test(xt)$expected[1,] # expected 'a'
a = G.test(xt[1,], p=ex/sum(ex))
ex = G.test(xt)$expected[2,] # expected 'b'
b = G.test(xt[2,], p=ex/sum(ex))
p.adjust(c(a$p.value, b$p.value), method="holm")



##
#### 6. Two-sample chi-squared test ####
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
mosaicplot( ~ X + Y, data=df, col=c("lightgreen","pink","lightyellow"), main="Y by X")

xt = xtabs( ~ X + Y, data=df) # make counts
View(xt)
chisq.test(xt) # omnibus test

## Post hoc tests of 2x2 table subsets
yn = chisq.test(xt[,c(1,2)]) # yes vs. no
ym = chisq.test(xt[,c(1,3)]) # yes vs. maybe
nm = chisq.test(xt[,c(2,3)]) # no vs. maybe
p.adjust(c(yn$p.value, ym$p.value, nm$p.value), method="holm")

# or, do all cellwise comparisons
chisq.multcomp(xt, p.method="holm") # xt shows levels
# to get the chi-Squared statistics, use qchisq(1-p, df=1),
# where p is the uncorrected (p.method="none“) pairwise p-value:
qchisq(1 - 4.5e-05, df=1) # 16.6479

# or, compare cells to expected frequencies using standardized residuals
chisq.posthoc.test(xt, method="holm")
# to get the chi-squared statistics, use qchisq(1-p, df=1),
# where p is the uncorrected (p.method="none“) p-value:
qchisq(1 - 0.004311, df=1) # 8.147944

# or, test each column against chance
y = chisq.test(xt[,1]) # yes
n = chisq.test(xt[,2]) # no
m = chisq.test(xt[,3]) # maybe
p.adjust(c(y$p.value, n$p.value, m$p.value), method="holm")

# or, test each row against chance
a = chisq.test(xt[1,]) # a
b = chisq.test(xt[2,]) # b
p.adjust(c(a$p.value, b$p.value), method="holm")

# or, test each column against expected frequencies
ex = chisq.test(xt)$expected[,1]
y = chisq.test(xt[,1], p=ex/sum(ex))
ex = chisq.test(xt)$expected[,2]
n = chisq.test(xt[,2], p=ex/sum(ex))
ex = chisq.test(xt)$expected[,3]
m = chisq.test(xt[,3], p=ex/sum(ex))
p.adjust(c(y$p.value, n$p.value, m$p.value), method="holm")

# or, test each row against expected frequencies
ex = chisq.test(xt)$expected[1,] # expected 'a'
a = chisq.test(xt[1,], p=ex/sum(ex))
ex = chisq.test(xt)$expected[2,] # expected 'b'
b = chisq.test(xt[2,], p=ex/sum(ex))
p.adjust(c(a$p.value, b$p.value), method="holm")



##
#### Dependent Samples ####
##

##
#### 7. Symmetry test ####
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
mosaicplot( ~ X + Y, data=df, col=c("beige","tan","pink"), main="Ice Cream by Season", xlab="Season")

xt = xtabs( ~ X + Y, data=df) # for later use
View(xt)
symmetry_test(Y ~ X | PId, data=df) # omnibus test

## Post hoc tests

# If we want to compare two seasons, we have dependent counts, because each
# respondent chose a flavor in each season.
# Define a function for convenience:
pairwise.symmetry.test <- function(s1, s2, data=df) {
  df2 <- df[df$X == s1 | df$X == s2,] # table subset
  df2$X = factor(df2$X) # update factor levels
  return (pvalue(symmetry_test(Y ~ X | PId, data=df2)))
}
fa.wi = pairwise.symmetry.test("fall", "winter", data=df)
fa.sp = pairwise.symmetry.test("fall", "spring", data=df)
fa.su = pairwise.symmetry.test("fall", "summer", data=df)
wi.sp = pairwise.symmetry.test("winter", "spring", data=df)
wi.su = pairwise.symmetry.test("winter", "summer", data=df)
sp.su = pairwise.symmetry.test("spring", "summer", data=df)
p.adjust(c(fa.wi, fa.sp, fa.su, wi.sp, wi.su, sp.su), method="holm")

# Sneak peek: Use a GLMM to conduct the omnibus test and pairwise comparisons.
# The results are quite similar to those from the omnibus and pairwise symmetry
# tests. See analysis #4 in GLMM.R for a similar case:
library(multpois)
library(lme4)
library(lmerTest)
m = glmer.mp(Y ~ X + (1|PId), data=df, control=glmerControl(optimizer="bobyqa"))
Anova.mp(m, type=3)
glmer.mp.con(m, pairwise ~ X, adjust="holm", control=glmerControl(optimizer="bobyqa"))


# Counts within single seasons (across flavors) are independent, because
# each respondent picked only one flavor per season.
# We can use a one-sample test:
fa = xmulti(xt[1,], rep(1/length(xt[1,]), length(xt[1,])), statName="Prob") # fall
wi = xmulti(xt[2,], rep(1/length(xt[2,]), length(xt[2,])), statName="Prob") # winter
sp = xmulti(xt[3,], rep(1/length(xt[3,]), length(xt[3,])), statName="Prob") # spring
su = xmulti(xt[4,], rep(1/length(xt[4,]), length(xt[4,])), statName="Prob") # summer
p.adjust(c(fa$pProb, wi$pProb, sp$pProb, su$pProb), method="holm")


# Counts within single flavors (across seasons) are dependent because the 
# same respondent could have chosen the same flavor in multiple seasons.
# Define a function for convenience:
pairwise.sign.test <- function(flavor, s1, s2, data=df) {
  df$chose.flavor.in.s1 = ifelse(df$Y == flavor & df$X == s1, 1, 0)
  df$chose.flavor.in.s2 = ifelse(df$Y == flavor & df$X == s2, 1, 0)
  return (pvalue(sign_test(chose.flavor.in.s1 ~ chose.flavor.in.s2, data=df)))
}
fa.wi = pairwise.sign.test("vanilla", "fall", "winter", data=df)
fa.sp = pairwise.sign.test("vanilla", "fall", "spring", data=df)
fa.su = pairwise.sign.test("vanilla", "fall", "summer", data=df)
wi.sp = pairwise.sign.test("vanilla", "winter", "spring", data=df)
wi.su = pairwise.sign.test("vanilla", "winter", "summer", data=df)
sp.su = pairwise.sign.test("vanilla", "spring", "summer", data=df)
p.adjust(c(fa.wi, fa.sp, fa.su, wi.sp, wi.su, sp.su), method="holm")

fa.wi = pairwise.sign.test("chocolate", "fall", "winter", data=df)
fa.sp = pairwise.sign.test("chocolate", "fall", "spring", data=df)
fa.su = pairwise.sign.test("chocolate", "fall", "summer", data=df)
wi.sp = pairwise.sign.test("chocolate", "winter", "spring", data=df)
wi.su = pairwise.sign.test("chocolate", "winter", "summer", data=df)
sp.su = pairwise.sign.test("chocolate", "spring", "summer", data=df)
p.adjust(c(fa.wi, fa.sp, fa.su, wi.sp, wi.su, sp.su), method="holm")

fa.wi = pairwise.sign.test("strawberry", "fall", "winter", data=df)
fa.sp = pairwise.sign.test("strawberry", "fall", "spring", data=df)
fa.su = pairwise.sign.test("strawberry", "fall", "summer", data=df)
wi.sp = pairwise.sign.test("strawberry", "winter", "spring", data=df)
wi.su = pairwise.sign.test("strawberry", "winter", "summer", data=df)
sp.su = pairwise.sign.test("strawberry", "spring", "summer", data=df)
p.adjust(c(fa.wi, fa.sp, fa.su, wi.sp, wi.su, sp.su), method="holm")


