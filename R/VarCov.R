###
### Statistical Inference in R
### Jacob O. Wobbrock, Ph.D.
### wobbrock@uw.edu
### The Information School
### University of Washington
### October 23, 2021
### Updated: 3/13/2025
###

###
### VarCov.R
### (Variance-covariance structures for linear mixed models)
###

library(nlme)     # for lme
library(lme4)     # for lmer
library(lmerTest)
library(MuMIn)    # for AICc
library(car)      # for Anova
library(emmeans)  # for emmeans


### R code for common covariance structures. 
### See ?nlme::corClasses, ?nlme::varClasses.
### See also https://rpubs.com/samuelkn/CovarianceStructuresInR
### See also https://www.ibm.com/docs/en/spss-statistics/30.0.0?topic=mixed-covariance-structure-list-command
### See also https://www.ibm.com/docs/en/spss-statistics/30.0.0?topic=statistics-covariance-structures

## Dummy data has one repeated factor X with three levels (a,b,c) and continuous response Y
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
row.names(df) <- 1:nrow(df) # renumber row names
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

# NOTES ON lme4::lmer()
#   The lme4::lmer function does not allow specifying common variance-covariance (VCV) structures 
#   for repeated factors or residuals. Therefore, we must use nlme::lme for this. For a list of 
#   common VCV structures, see 
#   https://www.ibm.com/docs/en/spss-statistics/30.0.0?topic=mixed-covariance-structure-list-command. 
#   For their matrix formulations, see 
#   https://www.ibm.com/docs/en/spss-statistics/30.0.0?topic=statistics-covariance-structures. For a 
#   treatment in R, see https://rpubs.com/samuelkn/CovarianceStructuresInR.
m.lmer = lmer(Y ~ X + (1|PId), data=df) # random intercept for each participant

# NOTES ON nlme::lme()
#  The correlation parameter sets covariances (matrix off-diagonal) and the weights parameter sets 
#  variances (matrix on-diagonal). When correlation=NULL or is unspecified, the off-diagonal values
#  are zero. When weights=NULL or is unspecified, the on-diagonal variances are equal. The R help pages 
#  called up with ?corClasses and ?varClasses explain these parameters. If nlme::lme fails to converge, 
#  try adding the "control" parameter to the lme call: 
#
#    control=list(maxIter=100, msMaxIter=100, niterEM=100, msMaxEval=100, opt="optim"). # Usually works!
#
#  For more information, see ?nlme::lmeControl.
#  
#  Note that the TPH fit, below, uses this control parameter to avoid a convergence error.

##
#### Variance-covariance structures ####
## ...even more available at ?nlme::corClasses
##

## ID. Scaled identity.
## All variances are equal, and all covariances are zero.
m.ID = lme(Y ~ X, random=~1|PId, data=df, na.action=na.omit)

## DIAG. Diagonal. This is a diagonal structure with heterogenous variance. Also called "Independence."
## All variances can differ; otherwise, like ID.
m.DIAG = lme(Y ~ X, random=~1|PId, data=df, na.action=na.omit, weights=varIdent(form=~1|X))

## CS. Compound symmetry. This structure has constant variance and constant covariance. Also called "Exchangeable."
## All variances are equal, and all covariances are equal.
m.CS = lme(Y ~ X, random=~1|PId, data=df, na.action=na.omit, correlation=corCompSymm(form=~1|PId))

## CSH. Heterogenous compound symmetry. This structure has non-constant variance and constant correlation.
## All variances can differ; otherwise, like CS.
m.CSH = lme(Y ~ X, random=~1|PId, data=df, na.action=na.omit, correlation=corCompSymm(form=~1|PId), weights=varIdent(form=~1|X))

## AR1. First-order autoregressive.
## All variances are equal, and all covariances decrease with distance.
m.AR1 = lme(Y ~ X, random=~1|PId, data=df, na.action=na.omit, correlation=corAR1(form=~1|PId))

## ARH1. Heterogenous first-order autoregressive.
## All variances can differ; otherwise, like AR1.
m.ARH1 = lme(Y ~ X, random=~1|PId, data=df, na.action=na.omit, correlation=corAR1(form=~1|PId), weights=varIdent(form=~1|X))

## ARMA11. Autoregressive moving average (1,1).
## All variances are equal, and all covariances decrease with distance, influenced by a moving average.
m.ARMA11 = lme(Y ~ X, random=~1|PId, data=df, na.action=na.omit, correlation=corARMA(form=~1|PId, p=1, q=1)) # (p,q)=(1,0) is AR1

## TP. Toeplitz. A more general version of AR1. 
## All variances are equal, and covariances are equal across adjacent pairs, equal again across skip-adjacent pairs, and so on.
m.TP = lme(Y ~ X, random=~1|PId, data=df, na.action=na.omit, correlation=corARMA(form=~1|PId, p=2, q=0))

## TPH. Heterogenous Toeplitz.
## All variances can differ; otherwise, like TP.
m.TPH = lme(Y ~ X, random=~1|PId, data=df, na.action=na.omit, correlation=corARMA(form=~1|PId, p=2, q=0), weights=varIdent(form=~1|X), control=list(maxIter=100, msMaxIter=100, niterEM=100, msMaxEval=100, opt="optim"))

## UN. Unstructured. A completely general covariance matrix.
## All variances and covariances can differ.
m.UN = lme(Y ~ X, random=~1|PId, data=df, na.action=na.omit, correlation=corSymm(form=~1|PId), weights=varIdent(form=~1|X))


# make a list of the 11 models we've built
models <- list(m.lmer, m.ID, m.DIAG, m.CS, m.CSH, m.AR1, m.ARH1, m.ARMA11, m.TP, m.TPH, m.UN)

#### Inspect the models ####
for (i in 1:length(models)) {
  print(paste0("-------------------- #", i, " --------------------"))
  print(summary(models[[i]]))
}

for (i in 1:length(models)) {
  print(paste0("-------------------- #", i, " --------------------"))
  print(VarCorr(models[[i]]))
}

#### Variance-covariance matrices ####
for (i in 2:length(models)) {
  print(paste0("-------------------- #", i, " --------------------"))
  print(getVarCov(models[[i]], type="random.effects"))
}
for (i in 2:length(models)) {
  print(paste0("-------------------- #", i, " --------------------"))
  print(getVarCov(models[[i]], type="conditional"))
}
for (i in 2:length(models)) {
  print(paste0("-------------------- #", i, " --------------------"))
  print(getVarCov(models[[i]], type="marginal"))
}

#### Information criteria ####
# lower is better
for (i in 1:length(models)) {
  print(paste0("-------------------- #", i, " --------------------"))
  print(-2*logLik(models[[i]]))
  print(paste0("AIC:  ", AIC(models[[i]]  )))
  print(paste0("AICc: ", AICc(models[[i]] )))
  print(paste0("BIC:  ", BIC(models[[i]]  )))
}

#### Significance tests ####
anova(m.lmer, type="I")
anova(m.lmer, type="II")
anova(m.lmer, type="III")
Anova(m.lmer, type=3, test.statistic="F") # only lmer produces "F", lme ignores
Anova(m.lmer, type=3, test.statistic="Chisq")

for (i in 2:length(models)) {
  print(paste0("-------------------- #", i, " --------------------"))
  print(anova(models[[i]], type="sequential")) # Type I ANOVA
}
for (i in 2:length(models)) {
  print(paste0("-------------------- #", i, " --------------------"))
  print(anova(models[[i]], type="marginal"))   # Type III ANOVA
}


#### Post hoc pairwise comparisons ####
## See https://cran.r-project.org/web/packages/emmeans/vignettes/models.html
# 
# Valid lme4::lmer() 'adjust' parameters:  
#   "tukey" (default), "holm", "scheffe", "sidak", "dunnettx", "mvt",  
#   "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
# 
# Valid lme4::lmer() df 'mode' parameters: 
#   “kenward-roger”, “satterthwaite”, “asymptotic”
emmeans(m.lmer, pairwise ~ X, adjust="holm", mode="kenward-roger")$contrasts # lme4::lmer model

# Valid nlme::lme() df 'mode' parameters: 
#   “auto” (default), “containment”, “satterthwaite”, “appx-satterthwaite”, 
#   “boot-satterthwaite”, “asymptotic”
for (i in 2:length(models)) {
  print(paste0("-------------------- #", i, " --------------------"))
  print(emmeans(models[[i]], pairwise ~ X, adjust="holm", mode="containment")$contrasts) # nlme::lme models
}


