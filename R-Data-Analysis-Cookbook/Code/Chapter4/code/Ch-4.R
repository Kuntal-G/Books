#Code snippets for Chapter 4 of R Data Analysis Cookbook


#Recipe: Computing the Root Mean Squared (RMS) error


dat <- read.csv("rmse-example.csv")
rmse <- sqrt(mean((dat$price-dat$pred)^2))
rmse

plot(dat$price, dat$pred, xlab = "Actual", ylab = "Predicted")
abline(0, 1)

rdacb.rmse <- function(actual, predicted) {
  return (sqrt(mean((actual-predicted)^2)))
}

rmse <- rdacb.rmse(dat$price, dat$pred)



# Recipe: Building KNN models for regression

library(dummies)
library(FNN)
library(scales)

educ <- read.csv("education.csv")

dums <- dummy(educ$region, sep="_")
educ <- cbind(educ, dums)

educ$urban.s <- rescale(educ$urban)
educ$income.s <- rescale(educ$income)
educ$under18.s <- rescale(educ$under18)

set.seed(1000)
t.idx <- createDataPartition(educ$expense, p = 0.6, list = FALSE)
trg <- educ[t.idx,]
rest <- educ[-t.idx,]
set.seed(2000)
v.idx <- createDataPartition(rest$expense, p=0.5, list=FALSE)
val <- rest[v.idx,]
test <- rest[-v.idx,]


res1 <- knn.reg(trg[, 7:12], val[,7:12], trg[,6], 1, algorithm="brute")
rmse1 = sqrt(mean((res1$pred-val[,6])^2))
rmse1

rmse1 = rdacb.rmse(res1$pred, val[,6])


res2 <- knn.reg(trg[, 7:12], val[,7:12], trg[,6], 2, algorithm="brute")
rmse2 = sqrt(mean((res2$pred-val[,6])^2))
rmse2

res3 <- knn.reg(trg[, 7:12], val[,7:12], trg[,6], 3, algorithm="brute")
rmse3 = sqrt(mean((res3$pred-val[,6])^2))
rmse3

res4 <- knn.reg(trg[, 7:12], val[,7:12], trg[,6], 4, algorithm="brute")
rmse4 = sqrt(mean((res4$pred-val[,6])^2))
rmse4

res.test <- knn.reg(trg[, 7:12], test[,7:12], trg[,6], 2, algorithm="brute")
rmse.test = sqrt(mean((res.test$pred-test[,6])^2))
rmse.test


t.idx <- createDataPartition(educ$expense, p = 0.7, list = FALSE)
trg <- educ[t.idx,]
val <- educ[-t.idx,]
res1 <- knn.reg(trg[,7:12], test = NULL, y = trg[,6], k=2, algorithm="brute")
rmse <- sqrt(mean(res1$residuals^2))

rdacb.knn.reg <- function (trg_predictors, val_predictors, trg_target, val_target, k) {
  library(FNN)
  res <- knn.reg(trg_predictors, val_predictors, trg_target,
                 k, algorithm = "brute")
  errors <- res$pred - val_target
  rmse <- sqrt(mean(errors * errors))
  cat(paste("RMSE for k=", toString(k), ":", sep = ""), rmse,
      "\n")
  rmse
}

set.seed(1000)
t.idx <- createDataPartition(educ$expense, p = 0.6, list = FALSE)
trg <- educ[t.idx,]
rest <- educ[-t.idx,]
set.seed(2000)
v.idx <- createDataPartition(rest$expense, p=0.5, list=FALSE)
val <- rest[v.idx,]
test <- rest[-v.idx,]
rdacb.knn.reg(trg[,7:12], val[,7:12], trg[,6], val[,6], 1)
rdacb.knn.reg(trg[,7:12], val[,7:12], trg[,6], val[,6], 2)

rdacb.knn.reg.multi <- function (trg_predictors, val_predictors, trg_target, val_target, start_k, end_k)
{
  rms_errors <- vector()
  for (k in start_k:end_k) {
    rms_error <- rdacb.knn.reg(trg_predictors, val_predictors,
                               trg_target, val_target, k)
    rms_errors <- c(rms_errors, rms_error)
  }
  plot(rms_errors, type = "o", xlab = "k", ylab = "RMSE")
}

rdacb.knn.reg.multi(trg[,7:12], val[,7:12], trg[,6], val[,6], 1, 5)

#Recipe: Performing linear regression

library(caret)

auto <- read.csv("auto-mpg.csv")

auto$cylinders <- factor(auto$cylinders, levels = c(3,4,5,6,8), labels = c("3cyl", "4cyl", "5cyl", "6cyl", "8cyl"))

set.seed(1000)
t.idx <- createDataPartition(auto$mpg, p = 0.7, list = FALSE)

names(auto)

mod <- lm(mpg ~ ., data = auto[t.idx, -c(1,8,9)])

mod

summary(mod)

pred <- predict(mod, auto[-t.idx, -c(1,8,9)])

sqrt(mean((pred - auto[-t.idx, 2])^2))

par(mfrow = c(2,2))
plot(mod)
par(mfrow = c(1,1))

auto <- within(auto, cylinders <- relevel(cylinders, ref = "4cyl") )
mod <- lm(mpg ~., data = auto[t.idx, -c(1, 8, 9)])



#Recipe: Performing variable selection in linear regression

library(caret)
library(MASS)

auto <- read.csv("auto-mpg.csv")

auto$cylinders <- factor(auto$cylinders, levels = c(3,4,5,6,8), labels = c("3cyl", "4cyl", "5cyl", "6cyl", "8cyl"))

set.seed(1000)
t.idx <- createDataPartition(auto$mpg, p = 0.7, list = FALSE)

names(auto)

fit <- lm(mpg ~ ., data = auto[t.idx, -c(1,8,9)])

step.model <- stepAIC(fit, direction = "backward")

summary(step.model)



#Recipe: Building regression trees

library(rpart)
library(rpart.plot)
library(caret)

bh <- read.csv("BostonHousing.csv")

set.seed(1000)
t.idx <- createDataPartition(bh$MEDV, p=0.7, list = FALSE)

bfit <- rpart(MEDV ~ ., data = bh[t.idx,])

bfit

prp(bfit, type=2, nn=TRUE, fallen.leaves=TRUE, faclen=4, varlen=8, shadow.col="gray")

bfit$cptable

plotcp(bfit)

# in the command below, replace the cp value based on your results
bfitpruned <- prune(bfit, cp=0.01192653)
prp(bfitpruned, type=2, nn=TRUE, fallen.leaves=TRUE, faclen=4, varlen=8, shadow.col="gray")

preds.t <- predict(bfitpruned, bh[t.idx,])
sqrt(mean((preds.t-bh[t.idx,"MEDV"])^2))

preds.v <- predict(bfitpruned, bh[-t.idx,])
sqrt(mean((preds.v - bh[-t.idx,"MEDV"])^2))

fit <- rpart(MEDV ~ ., data = bh[t.idx,], control = rpart.control(minsplit = 10, cp = 0.001, minbucket = 5))

ed <- read.csv("education.csv")
ed$region <- factor(ed$region)
set.seed(1000)
t.idx <- createDataPartition(ed$expense, p = 0.7, list = FALSE)
fit <- rpart(expense ~ region+urban+income+under18, data = ed[t.idx,])
prp(fit, type=2, nn=TRUE, fallen.leaves=TRUE, faclen=4, varlen=8, shadow.col="gray")



#ensemble methods- bagging and boosting
install.packages("ipred")
library(ipred)
# using same BostonHousing.csv dataset used earlier
fit <- bagging(MEDV ~., data= bh[t.idx,], control= rpart.control(minsplit=10))
prediction.t <- predict(fit, bh[t.idx,]) # on training set
sqrt(mean((prediction.t-bh[t.idx,"MEDV"])^2))# RMS for training

install.packages("gbm")
library(gbm)
# using same BostonHousing.csv dataset used earlier
gbmFit <- gbm(MEDV ~., data= bh, distribution="gaussian")
prediction.t <- predict(gbmFit, bh) # on training set
sqrt(mean((prediction.t-bh$MEDV)^2))# RMS for training

#Recipe: Building random forest models for regression
#-----------------------------------------------------------------
library(randomForest)
library(caret)

bn <- read.csv("BostonHousing.csv")
set.seed(1000)
t.idx <- createDataPartition(bh$MEDV, p=0.7, list=FALSE)

mod <- randomForest(x = bh[t.idx,1:13], y=bh[t.idx,14],ntree=1000,  xtest = bh[-t.idx,1:13], ytest = bh[-t.idx,14], importance=TRUE, keep.forest=TRUE)

mod

mod$importance

plot(bh[t.idx,14], predict( mod, newdata=bh[t.idx,]), xlab = "Actual", ylab = "Predicted")

plot(bh[t.idx,14], mod$predicted, xlab = "Actual", ylab = "Predicted")

plot(bh[-t.idx,14], mod$test$predicted, xlab = "Actual", ylab = "Predicted")


#Recipe: Using neural network models for regression
#----------------------------------------------------------------
library(nnet)
library(caret)
library(devtools)

bh <- read.csv("BostonHousing.csv")

set.seed(1000)
t.idx <- createDataPartition(bh$MEDV, p=0.7, list=FALSE)

summary(bh$MEDV)

fit <- nnet(MEDV/50 ~ ., data=bh[t.idx,], size=6, decay = 0.1, maxit = 1000, linout = TRUE)

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

plot(fit, max.sp = TRUE)

t.rmse = sqrt(mean((fit$fitted.values*50 - bh[t.idx, "MEDV"])^2))
t.rmse

v.rmse <- sqrt(mean((predict(fit,bh[-t.idx,]*50 - bh[-t.idx, "MEDV"])^2)))
v.rmse


#Recipe: Performing k-fold cross validation
#----------------------------------------
bh <- read.csv("BostonHousing.csv")

rdacb.kfold.crossval.reg <- function(df, nfolds) {
  fold <- sample(1:nfolds, nrow(df), replace = TRUE)
  mean.sqr.errs <- sapply(1:nfolds, rdacb.kfold.cval.reg.iter,
         df, fold)
  list("mean_sqr_errs"= mean.sqr.errs,
         "overall_mean_sqr_err" = mean(mean.sqr.errs),
         "std_dev_mean_sqr_err" = sd(mean.sqr.errs))
}


 rdacb.kfold.cval.reg.iter <- function(k, df, fold) {
   trg.idx <- !fold %in% c(k)
   test.idx <-  fold %in% c(k)
   mod <- lm(MEDV ~ ., data = df[trg.idx, ] )
   pred <- predict(mod, df[test.idx,])
   sqr.errs <- (pred - df[test.idx, "MEDV"])^2
   mean(sqr.errs)
 }

 res <- rdacb.kfold.crossval.reg(bh, 5)

res$mean_sqr_errs

res$overall_mean_sqr_err
res$std_dev_mean_sqr_err

#Recipe: Performing leave-one-out-cross-validation (LOOCV)
#------------------------------------------------------
bh <- read.csv("BostonHousing.csv")

 rdacb.loocv.reg <- function(df) {
   mean.sqr.errs <- sapply(1:nrow(df),
               rdacb.loocv.reg.iter, df)
   list("mean_sqr_errs"= mean.sqr.errs,
              "overall_mean_sqr_err" = mean(mean.sqr.errs),
              "std_dev_mean_sqr_err" = sd(mean.sqr.errs))
 }


 rdacb.loocv.reg.iter <- function(k, df) {
    mod <- lm(MEDV ~ ., data = df[-k, ] )
    pred <- predict(mod, df[k,])
    sqr.err <- (pred - df[k, "MEDV"])^2
  }

res <- rdacb.loocv.reg(bh)
res$mean_sqr_errs
res$overall_mean_sqr_err
res$std_dev_mean_sqr_err
