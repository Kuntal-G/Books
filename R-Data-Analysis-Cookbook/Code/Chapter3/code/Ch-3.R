
#Code snippets for Chapter 3 of R Data analysis cookbook
#=======================================================

#Recipe: Generating error/classification-confusion matrix
#------------------------------------------------------

cp <- read.csv("college-perf.csv")
cp$Perf <- ordered(cp$Perf, levels = c("Low", "Medium", "High"))

cp$Pred <- ordered(cp$Pred, levels =  c("Low", "Medium", "High"))

tab <- table(cp$Perf, cp$Pred, dnn = c("Actual", "Predicted"))
tab

prop.table(tab)

round(prop.table(tab, 1)*100, 1)

barplot(tab, legend = TRUE)

mosaicplot(tab, main = "Prediction performance")

summary(tab)

#Recipe:Pricipal Component Analysis
library(stats)

usArrests=read.csv("USArrests.csv",stringsAsFactors = FALSE)
rownames(usArrests)=usArrests$X
usArrests$X=NULL
head(usArrests)
apply(usArrests , 2, var)

pca =prcomp(usArrests ,center = TRUE, scale =TRUE)na
print(pca)
plot(pca, type='l')

pca$rotation=-pca$rotation
pca$x=-pca$x
biplot (pca , scale =0)

#Recipe: Generating ROC Charts
#-------------------------------

library(ROCR)

dat <- read.csv("roc-example-1.csv")

head(dat)

pred <- prediction(dat$prob, dat$class)

perf <- performance(pred, "tpr", "fpr")

plot(perf)
lines( par()$usr[1:2], par()$usr[3:4] )

prob.cuts <- data.frame(cut=perf@alpha.values[[1]], 
   fpr=perf@x.values[[1]], tpr=perf@y.values[[1]])

head(prob.cuts)

tail(prob.cuts)

dat <- read.csv("roc-example-2.csv")
pred <- prediction(dat$prob, dat$class, label.ordering = c("non-buyer", "buyer"))
perf <- performance(pred, "tpr", "fpr")
plot(perf)
lines( par()$usr[1:2], par()$usr[3:4] )

#Recipe: Building plotting and evaluating classification trees
#------------------------------------------------------

library(rpart)
library(rpart.plot)
library(caret)

bn <- read.csv("banknote-authentication.csv")
set.seed(1000)
train.idx <- createDataPartition(bn$class, p = 0.7, list = FALSE)

mod <- rpart(class ~ ., data = bn[train.idx, ], method = "class", 
  control = rpart.control(minsplit = 20, cp = 0.01))
  
mod

prp(mod, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, 
  faclen = 4, varlen = 8, shadow.col = "gray")
  
mod$cptable

# Replace 5 on the following line with the appropriate value for your data
mod.pruned = prune(mod, mod$cptable[5, "CP"])

prp(mod.pruned, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, 
  faclen = 4, varlen = 8, shadow.col = "gray")
  
pred.pruned <- predict(mod, bn[-train.idx,], type = "class")

table(bn[-train.idx,]$class, pred.pruned, dnn = c("Actual", "Predicted"))


pred.pruned <- predict(mod, bn[-train.idx,], type = "prob")

pred <- prediction(pred.pruned[,2], bn[-train.idx,"class"])
perf <- performance(pred, "tpr", "fpr")
plot(perf)

#Recipe: Using random forest models for classification
#-------------------------------------------------

library(randomForest)
library(caret)
library(ROCR)

bn <- read.csv("banknote-authentication.csv")
bn$class <- factor(bn$class)

set.seed(1000)
sub.idx <- createDataPartition(bn$class, p=0.7, list=FALSE)

mod <- randomForest(x = bn[sub.idx,1:4], y=bn[sub.idx,5],ntree=500, keep.forest=TRUE)

pred <- predict(mod, bn[-sub.idx,])

table(bn[-sub.idx,"class"], pred, dnn = c("Actual", "Predicted"))

probs <- predict(mod, bn[-sub.idx,], type = "prob")

pred <- prediction(probs[,2], bn[-sub.idx,"class"])
perf <- performance(pred, "tpr", "fpr")
plot(perf)

#Recipe: Classifying using support vector machines
#-----------------------------------------------

library(e1071)
library(caret)

bn <- read.csv("banknote-authentication.csv")

bn$class <- factor(bn$class)

set.seed(1000)
t.idx <- createDataPartition(bn$class, p=0.7, list=FALSE)

mod <- svm(class ~ ., data = bn[t.idx,])

table(bn[t.idx,"class"], fitted(mod), dnn = c("Actual", "Predicted"))

pred <- predict(mod, bn[-t.idx,])
table(bn[-t.idx, "class"], pred, dnn = c("Actual", "Predicted"))

plot(mod, data=bn[t.idx,], skew ~ variance)

plot(mod, data=bn[-t.idx,], skew ~ variance)

mod <- svm(class ~ ., data = bn[t.idx,], class.weights=c("0"=0.3, "1"=0.7 ))

# Large margin allowing more misclassification
mod <- svm(class ~ ., data = bn[t.idx,], cost = 1) 

# Narrow margin permitting few misclassification
mod <- svm(class ~ ., data =bn[t.idx,], cost = 1000)

tuned = tune.svm(class ~ ., data = bn[t.idx,], gamma = 10^(-6:-1),
                 cost = 10^(1:2))
summary(tuned)



#Recipe: Classifying using the Naïve Bayes approach
#------------------------------------------------

library(e1071)
library(caret)

ep <- read.csv("electronics-purchase.csv")

set.seed(1000)
train.idx <- createDataPartition(ep$Purchase, p = 0.67, list = FALSE)

epmod <- naiveBayes(Purchase ~ . , data = ep[train.idx,])

epmod

pred <- predict(epmod, ep[-train.idx,])

tab <- table(ep[-train.idx,]$Purchase, pred, dnn = c("Actual", "Predicted"))

tab

#Lastly, you can generate a confusion matrix from the classification table:
confusionMatrix(tab)

#Recipe: Classifying using the K-Nearest Neighbors (KNN) approach
#----------------------------------------------------------------

library(class)
library(caret)

vac <- read.csv("vacation-trip-classification.csv")

vac$Income.z <- scale(vac$Income)
vac$Family_size.z <- scale(vac$Family_size)

set.seed(1000)
train.idx <- createDataPartition(vac$Result, p = 0.5, list = FALSE)

train <- vac[train.idx, ]

temp <- vac[-train.idx, ]

val.idx <- createDataPartition(temp$Result, p = 0.5, list = FALSE)

val <- temp[val.idx, ]

test <- temp[-val.idx, ]

pred1 <- knn(train[,4:5], val[,4:5], train[,3], 1)

errmat1 = table(val$Result, pred1, dnn = c("Actual", "Predicted"))

pred.test <- knn(train[,4:5], test[,4:5], train[,3], 1)

errmat.test = table(test$Result, pred.test, dnn = c("Actual", "Predicted"))

knn.automate <- function (trg_predictors, val_predictors, trg_target, val_target, start_k, end_k) 
{
  for (k in start_k:end_k) {
    pred <- knn(trg_predictors, val_predictors, 
                               trg_target, k)
    tab <- table(val_target, pred, dnn = c("Actual", "Predicted"))
    cat(paste("Error matrix for k=", k,"\n"))
    cat("==========================\n")
    print(tab)
    cat("--------------------------\n\n\n")
  }
}


knn.automate(train[,4:5], val[,4:5], train[,3], val[,3], 1,7)

pred5 <- knn(train[4:5], val[,4:5], train[,3], 5, prob=TRUE)

pred5

# Using caret to find out best value of k

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

caret_knn_fit <- train(Result ~  Family_size + Income, data = train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

#Recipe: Using neural networks for classification
#--------------------------------------

library(nnet)
library(caret)

bn <- read.csv("banknote-authentication.csv")

bn$class <- factor(bn$class)


train.idx <- createDataPartition(bn$class, p=0.7, list = FALSE)

mod <- nnet(class ~., data=bn[train.idx,],size=3,maxit=10000,decay=.001, rang = 0.05)

pred <- predict(mod, newdata=bn[-train.idx,], type="class")

table(bn[-train.idx,]$class, pred)

pred <- predict(mod, newdata=bn[-train.idx,], type="raw")


#Recipe: Classifying using Linear Discriminant Function Analysis
#--------------------------------------------------------------

library(MASS)
library(caret)

bn <- read.csv("banknote-authentication.csv")

bn$class <- factor(bn$class)

set.seed(1000)
t.idx <- createDataPartition(bn$class, p = 0.7, list=FALSE)

ldamod <- lda(bn[t.idx, 1:4], bn[t.idx, 5])

bn[t.idx,"Pred"] <- predict(ldamod, bn[t.idx, 1:4])$class

table(bn[t.idx, "class"], bn[t.idx, "Pred"], dnn = c("Actual", "Predicted"))

bn[-t.idx,"Pred"] <- predict(ldamod, bn[-t.idx, 1:4])$class

table(bn[-t.idx, "class"], bn[-t.idx, "Pred"], dnn = c("Actual", "Predicted"))

ldamod <- lda(class ~ ., data = bn[t.idx,])

#Recipe: Classifying using Logistic Regression
#--------------------------------------------

library(caret)

bh <- read.csv("boston-housing-logistic.csv")

bh$CLASS <- factor(bh$CLASS, levels = c(0,1))

set.seed(1000)
train.idx <- createDataPartition(bh$CLASS, p=0.7, list = FALSE)

logit <- glm(CLASS~., data = bh[train.idx,], family=binomial)

summary(logit)

bh[-train.idx,"PROB_SUCC"] <- predict(logit, newdata = bh[-train.idx,], type="response")

bh[-train.idx,"PRED_50"] <- ifelse(bh[-train.idx, "PROB_SUCC"]>= 0.5, 1, 0)

table(bh[-train.idx, "CLASS"], bh[-train.idx, "PRED_50"], dnn=c("Actual", "Predicted"))

#Recipe: Text Classification for Sentiment Analysis
#---------------------------------------------------

install.packages(c("twitteR","RColorBrewer","plyr","ggplot2","devtools","httr"))

require(devtools)

install_url("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")

install_url("https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz")

install_url("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")



library(slam)
library(sentiment)
library(twitteR)
library(RColorBrewer)
library(ggplot2)

api_key <- "xgBgAyuBo8Fpmpj9n1W47jQEW"

api_secret <- "0dc8sIs1ExWltVnXC20j5Y225Qa81tS4x4oyj1fBiicOkEHSEC"

access_token <- "1127738809-i11uTmZrRHRjHMQk5MhIG1nWloX9zDc7GKal0bA"

access_token_secret <- "Oy7AJ0DcQqT8wprLIzLw2aMBL9aouXvSxOd5JiMBpOPtV"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

MachineLearning_tweets = searchTwitter("machinelearning", n=1500, lang="en") 
MachineLearning_text = sapply(MachineLearning_tweets, function(x) x$getText())

clean.data = function(text) {
  
  # delete re-tweet entries
  text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
  # remove @ word
  text = gsub("@\\w+", "", text)
  # delete punctuation
  text = gsub("[[:punct:]]", "", text )
  # remove Digits: 0 1 2 3 4 5 6 7 8 9
  text = gsub("[[:digit:]]", "", text)
  # delete html links
  text = gsub("http\\w+", "", text)
  # delete unnecessary spaces like tab and space
  text = gsub("[ \t]{2,}", "", text)
  text = gsub("^\\s+|\\s+$", "", text)
  
  return(text)
}

MachineLearning_text= clean.data(MachineLearning_text)

handle.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

MachineLearning_text = sapply(MachineLearning_text, handle.error)
MachineLearning_text = MachineLearning_text [!is.na(MachineLearning_text)]
names(MachineLearning_text) = NULL
class_emo = classify_emotion(MachineLearning_text, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown" 

class_pol = classify_polarity(MachineLearning_text, algorithm="bayes")

polarity = class_pol[,4]
sent_df = data.frame(text= MachineLearning_text, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


ggplot(sent_df, aes(x=emotion)) +
  
  geom_bar(aes(y=..count.., fill=emotion)) +
  
  scale_fill_brewer(palette="Set2") +
  
  labs(x="emotion categories", y="number of tweets") +
  
  labs(title = "Sentiment Analysis about Machine Learning")


ggplot(sent_df, aes(x=polarity)) +
  
  geom_bar(aes(y=..count.., fill=polarity)) +
  
  scale_fill_brewer(palette="Set3") +
  
  labs(x="polarity categories", y="number of tweets") +
  
  labs(title = "Sentiment Analysis about Machine Learning")
