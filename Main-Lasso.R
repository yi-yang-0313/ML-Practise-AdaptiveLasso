
## This file executes the adaptive Lasso analysis of variable selection

# Loading the library

library(glmnet)
library(readxl)
library(tidyverse) #install.packages('tidyverse')
library(magrittr)
library(glmnet)
library(pROC) #install.packages('pROC')

set.seed(19940313)

# Loading the data

data_for_lasso <- read_excel("~/GoogleDrive/1. Research/project - Education Inefficiency/Data/Merged Data/ALasso-Data.xls")
comp.lasso <- data.frame(lapply(data_for_lasso, as.double))
# Find the optimal value of lambda that minimizes the cross-validation error

comp.lasso <- arrange(comp.lasso, desc(Year))


x.16 <- as.matrix(comp.lasso[c(1:425),c(3:19)]) # Take out the X 
y.math.16 <- as.matrix(comp.lasso[c(1:425),c(1)]) # Take Out the Y
y.ela.16 <- as.matrix(comp.lasso[c(1:425),c(2)])

x.15 <- as.matrix(comp.lasso[c(426:850),c(3:19)]) # Take out the X 
y.math.15 <- as.matrix(comp.lasso[c(426:850),c(1)]) # Take Out the Y
y.ela.15 <- as.matrix(comp.lasso[c(426:850),c(2)])

x.14 <- as.matrix(comp.lasso[c(851:1275),c(3:19)]) # Take out the X 
y.math.14 <- as.matrix(comp.lasso[c(851:1275),c(1)]) # Take Out the Y
y.ela.14 <- as.matrix(comp.lasso[c(851:1275),c(2)])


# Math x Regular Lasso

cv.lasso <- cv.glmnet(x.16, y.math.16, alpha = 1, nfolds=3)

plot(cv.lasso)
plot(cv.lasso, xvar="lambda")
best.lam <- cv.lasso$lambda.min # calculate best lambda
best.lam
#lse is less accurate but reduces more coefficients
#the value of lambda that gives the simplest model but also lies within one standard error of the optimal value of lambda. 

coef(cv.lasso, cv.lasso$lambda.min)
#coef(cv.lasso, cv.lasso$lambda.1se)

# Optimal Model
lasso.best <- glmnet(x.14, y.math.14, alpha = 1, lambda = best.lam)
lasso.best

#Prediction
pred <- predict(lasso.best, s = best.lam, newx = x.lasso)
final.lasso <- cbind(y.math.lasso, pred)
head(final.lasso)
#### Inspecting beta coefficients
coef(lasso.best)

## ELA x Lasso 

cv.lasso.ela <- cv.glmnet(x.lasso, y.ela.lasso, alpha = 1, nfolds=3)

plot(cv.lasso.ela)
plot(cv.lasso.ela, xvar="lambda")
best.lam.ela <- cv.lasso.ela$lambda.min

best.lam.ela
#lse is less accurate but reduces more coefficients
#the value of lambda that gives the simplest model but also lies within one standard error of the optimal value of lambda. 
# best.lam is 0.019214 when nfold=3
#cv.lasso$lambda.1se
coef(cv.lasso.ela, cv.lasso.ela$lambda.min)
#coef(cv.lasso, cv.lasso$lambda.1se)




## Adaptive Lasso - Math


# “Glmnet is a package that fits a generalized linear model via penalized 
# maximum likelihood. The regularization path is computed for the lasso or
# elasticnet penalty at a grid of values for the regularization parameter 
# lambda. The algorithm is extremely fast, and can exploit sparsity in the
# input matrix x. It fits linear, logistic and multinomial, poisson, and 
# Cox regression models. A variety of predictions can be made from the 
# fitted models. It can also fit multi-response linear regression.”



# OLS 
ols.regression <- 1
if (ols.regression ==1){
comp.alasso.16 <- data.frame(y.math.16, x.16)
colnames(comp.alasso.16)[1]<-"Gain"
ols.results.16 <- lm(Gain ~.,data=comp.alasso.16)
ols.coef.16 <- as.numeric(coef(ols.results.16))[-1]
}

# Ridge 
ridge.regression <- 0
if (ridge.regression ==1){
ridge.results <- cv.glmnet(x = x.lasso, y = y.math.lasso, type.measure = "mse", nfold=3, 
                            ## type.measure: loss to use for cross-validation.
                            ## ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
                            alpha = 0,
                            ##
                            ## penalty.factor: Separate penalty factors can be applied to each
                            ##           coefficient. This is a number that multiplies ‘lambda’ to
                            ##           allow differential shrinkage. Can be 0 for some variables,
                            ##           which implies no shrinkage, and that variable is always
                            ##           included in the model. Default is 1 for all variables (and
                            ##           implicitly infinity for variables listed in ‘exclude’). Note:
                            ##           the penalty factors are internally rescaled to sum to nvars,
                            ##           and the lambda sequence will reflect this change.
                            #Default Keep is False. I experiemented with True and find no difference.
)

plot(ridge.results)
ridge.results$lambda.min
coef(ridge.results, s = ridge.results$lambda.min)
best.ridge.coef <- as.numeric(coef(ridge.results, s = ridge.results$lambda.min))[-1]
}


# Cross Validation for Adaptive Lasso
alasso.results.16 <- cv.glmnet(x = x.16, y = y.math.16, type.measure = "mse", nfold=3, 
                  ## type.measure: loss to use for cross-validation.
                  ## ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
                  alpha = 1,
                  ##
                  ## penalty.factor: Separate penalty factors can be applied to each
                  ##           coefficient. This is a number that multiplies ‘lambda’ to
                  ##           allow differential shrinkage. Can be 0 for some variables,
                  ##           which implies no shrinkage, and that variable is always
                  ##           included in the model. Default is 1 for all variables (and
                  ##           implicitly infinity for variables listed in ‘exclude’). Note:
                  ##           the penalty factors are internally rescaled to sum to nvars,
                  ##           and the lambda sequence will reflect this change.
                  penalty.factor = 1 / abs(ols.coef.16),
                  #penalty.factor = 1 / abs(best.ridge.coef),
                  keep = FALSE
                  #Default Keep is False. I experiemented with True and find no difference.
                  )

plot(alasso.results.16)
plot(alasso.results.16, xvar = "lambda")
alasso.lam.16 <- alasso.results.16$lambda.min
alasso.lam.16
# the lambda is 0.00079

## Results
best.alasso.coef.16 <- coef(alasso.results.16, s = alasso.lam.16 )
#best.alasso.coef.alter <- coef(alasso.results, s = alasso.results$lambda.1se )

best.alasso.coef.16
#best.alasso.coef.alter
################################ Calculate BIC

# Adaptive Lasso x ELA

# OLS 
ols.regression,.ela <- 1
if (ols.regression.ela ==1){
  comp.alasso.ela <- data.frame(y.ela.lasso, x.lasso)
  colnames(comp.alasso.ela)[1]<-"Gain"
  ols.results.ela <- lm(Gain ~.,data=comp.alasso.ela)
  ols.coef.ela <- as.numeric(coef(ols.results.ela))[-1]
}

## Cross Validation for Adaptive Lasso

alasso.results.ela <- cv.glmnet(x = x.lasso, y = y.ela.lasso, type.measure = "mse", nfold=3, 
                            ## type.measure: loss to use for cross-validation.
                            ## ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
                            alpha = 1,
                            ##
                            ## penalty.factor: Separate penalty factors can be applied to each
                            ##           coefficient. This is a number that multiplies ‘lambda’ to
                            ##           allow differential shrinkage. Can be 0 for some variables,
                            ##           which implies no shrinkage, and that variable is always
                            ##           included in the model. Default is 1 for all variables (and
                            ##           implicitly infinity for variables listed in ‘exclude’). Note:
                            ##           the penalty factors are internally rescaled to sum to nvars,
                            ##           and the lambda sequence will reflect this change.
                            penalty.factor = 1 / abs(ols.coef.ela),
                            #penalty.factor = 1 / abs(best.ridge.coef),
                            keep = FALSE
                            #Default Keep is False. I experiemented with True and find no difference.
)

plot(alasso.results.ela)
plot(alasso.results.ela, xvar = "lambda")
alasso.lam.ela <- alasso.results.ela$lambda.min
alasso.lam.ela
# the lambda is 0.00079

## Results
best.alasso.coef.ela <- coef(alasso.results.ela, s = alasso.lam.ela )
#best.alasso.coef.alter <- coef(alasso.results, s = alasso.results$lambda.1se )

best.alasso.coef.ela
#best.alasso.coef.alter



## Useful References

# https://www.rstatisticsblog.com/data-science-in-action/lasso-regression/
# http://www.sthda.com/english/articles/36-classification-methods-essentials/149-penalized-logistic-regression-essentials-in-r-ridge-lasso-and-elastic-net/
# https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
# Adaptive Lasso Practive https://rpubs.com/kaz_yos/alasso



