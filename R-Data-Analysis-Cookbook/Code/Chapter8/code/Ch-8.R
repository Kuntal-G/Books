#Collaborative filtering
#==================================
install.packages("recommenderlab")
library(recommenderlab)
data(MovieLense)

ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                               colCounts(MovieLense) > 100]

which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_movies),
                        replace = TRUE, prob = c(0.8, 0.2))
recc_data_train <- ratings_movies[which_train, ]
recc_data_test <- ratings_movies[!which_train, ]



## Item Based
ibcf_recc_model <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(k = 30))
ibcf_model_details <- getModel(ibcf_recc_model)
ibcf_model_details

n_recommended <- 6
ibcf_recc_predicted <- predict(object = ibcf_recc_model, newdata = recc_data_test, n = n_recommended)
ibcf_recc_predicted

ibcf_recc_matrix <- sapply(ibcf_recc_predicted@items, function(x){ colnames(ratings_movies)[x]
})

View(ibcf_recc_matrix[, 1:4])

## User Based

ubcf_recc_model <- Recommender(data = recc_data_train, method = "UBCF")
ubcf_model_details <- getModel(ubcf_recc_model)
ubcf_model_details

n_recommended <- 5
ubcf_recc_predicted <- predict(object = ubcf_recc_model, newdata = recc_data_test, n = n_recommended)
ubcf_recc_predicted

ubcf_recc_matrix <- sapply(ubcf_recc_predicted@items, function(x){ colnames(ratings_movies)[x] })
View(ubcf_recc_matrix[, 1:4])


recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models)


image(MovieLense, main = "Heatmap of the rating matrix")

min_n_movies <- quantile(rowCounts(MovieLense), 0.99)
min_n_users <- quantile(colCounts(MovieLense), 0.99)
image(MovieLense[rowCounts(MovieLense) > min_n_movies,colCounts(MovieLense) > min_n_users], main ="Heatmap of the top users and movies")

ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,colCounts(MovieLense) > 100]
min_movies <- quantile(rowCounts(ratings_movies), 0.98)

min_users <- quantile(colCounts(ratings_movies), 0.98)
image(ratings_movies[rowCounts(ratings_movies) > min_movies,colCounts(ratings_movies) > min_users], main = "Heatmap of the top users and movies")




ratings_movies_viewed <- binarize(ratings_movies, minRating = 1)

which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_movies_viewed),
                      replace = TRUE, prob = c(0.8, 0.2))
recc_data_train <- ratings_movies_viewed [which_train, ]
recc_data_test <- ratings_movies_viewed [!which_train, ]

recc_model <- Recommender(data = recc_data_train, method = "IBCF",
                          parameter = list(method = "Jaccard"))
model_details <- getModel(recc_model)


n_recommended <- 6
recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended)
recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_movies)[x]
})

View(recc_matrix[, 1:4])




# Content based Filtering
#==================================

movie_URL <- "http://files.grouplens.org/datasets/movielens/ml-100k/u.item"
movieTitleDF <- read.table(movie_URL, header = F, sep = "|", quote = "\"")

names(movieTitleDF) <- c("MovieID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
movieTitleDF$ReleaseDate <- NULL; 
movieTitleDF$VideoReleaseDate <- NULL
movieTitleDF$IMDB <- NULL
movieTitleDF <- unique(movieTitleDF)
str(movieTitleDF)

users_URL <- "http://files.grouplens.org/datasets/movielens/ml-100k/u.data"
userDF <- read.table(users_URL, header = F, sep = "\t", quote = "\"")
names(userDF) <- c("UserID", "ItemID", "Rating")
userDF <- userDF[,1:3]
str(userDF)


clusterMovies<-function(movieTitleDF){
  set.seed(123)
  i<-1
  #get rid of movie ids and titles
  movieTitleDF<-movieTitleDF[,c(-1,-2)]
  movieCluster <- kmeans(movieTitleDF, 10, nstart = 20)
  return(movieCluster)
}

getUserInfo<-function(dat,id){
  a<-subset(dat, UserID==id,select=c(ItemID, Rating))
  # allocate 0 to the cluster column
  cluster<-0
  activeUser <- data.frame( a[order(a$ItemID),] ,cluster)
  return(activeUser)
}

setUserMovieCluster<-function(movieCluster, activeUser){
  df1<- data.frame(cbind(movieTitleDF$MovieID, clusterNum = movieCluster$cluster))
  names(df1)<-c("movie_id", "cluster")
  activeUser$cluster<-df1[match(activeUser$ItemID, df1$movie_id),2]
  return(activeUser)
}

getAverageClusterRating<-function(movieCluster, activeUser){
  like<-aggregate(activeUser$Rating, by=list(cluster=activeUser$cluster), mean)
  if(max(like$x)<3){
    like<-as.vector(0)
   } else{
    like<-as.vector(t(max(subset(like, x>=3, select=cluster))))
  }
  return(like)
}

getGoodMovies<-function(like, movieCluster, movieTitleDF){
  df1<- data.frame(cbind(movieTitleDF$MovieID, clusterNum = movieCluster$cluster))
  names(df1)<-c("movie_id", "cluster")
  if(like==0){
    recommend<-movieTitleDF[sample.int(n = dim(titleFilmDF)[1], size = 100), 1]
  }
  else{
    recommend<-as.vector(t(subset(df1, cluster==like, select=movie_id)))
  }
  return(recommend)
}


getRecommendedMovies<-function(movieTitleDF, userDF, userid){
  movieCluster<-clusterMovies(movieTitleDF)
  activeUser<-getUserInfo(userDF, userid)
  activeUser<-setUserMovieCluster(movieCluster, activeUser)
  like<-getAverageClusterRating(movieCluster, activeUser)
  recommend<-getGoodMovies(like, movieCluster, movieTitleDF)
  # only select not yet watched movies
  recommend<-recommend[-activeUser$ItemID]
  mov_title<-movieTitleDF[match(recommend,movieTitleDF$MovieID),2]
  recommend<-data.frame(recommend,mov_title)
  return(recommend)
}

suggestMovies<-function(movieTitleDF, userDF, userid, num_movies){
  #get suggestions
  suggestions = getRecommendedMovies(movieTitleDF, userDF, userid)
  #select stated number of selections
  suggestions = suggestions[1:num_movies,]
  writeLines("You may also like these movies:")
  #print suggestions without column headers or row indices
  write.table(suggestions[2], row.names = FALSE, col.names = FALSE)
}

suggestMovies(movieTitleDF, userDF, 196, 5)


# Hybrid Recommender
#==================================
data("MovieLense")
MovieLense50 <- MovieLense[rowCounts(MovieLense) >50,]
train <- MovieLense50[1:100]
test <- MovieLense50[101:105]

hybrid_recom <- HybridRecommender(
  Recommender(train, method = "UBCF"),
  Recommender(train, method = "RANDOM"),
  weights = c(.7,.3)
)

hybrid_recom

getModel(hybrid_recom)

as(predict(hybrid_recom, test, n=5), "list")

# Model Evaluation and Optimization
#==================================
install.packages("recommenderlab")
install.packages("ggplot2 ")

library(recommenderlab)
library(ggplot2)

data(MovieLense)
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                             colCounts(MovieLense) > 100]
ratings_movies

n_fold <- 4
items_to_keep <- 15
rating_threshold <- 3
eval_sets <- evaluationScheme(data = ratings_movies, method = "cross-validation",k = n_fold, given = items_to_keep, goodRating = rating_threshold)

size_sets <- sapply(eval_sets@runsTrain, length)
size_sets

model_to_evaluate <- "IBCF"
model_parameters <- NULL
eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate, parameter = model_parameters)

items_to_recommend <- 10

eval_prediction <- predict(object = eval_recommender, newdata =
                             getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")
class(eval_prediction)

qplot(rowCounts(eval_prediction)) + geom_histogram(binwidth = 20) +
  ggtitle("Distribution of movies per user")

eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =
    TRUE)
head(eval_accuracy)

qplot(eval_accuracy[, "RMSE"]) + geom_histogram(binwidth = 0.1) +
  ggtitle("Distribution of the RMSE by user")


results <- evaluate(x = eval_sets, method = model_to_evaluate, n =
                      seq(10, 100, 10))
class(results)

head(getConfusionMatrix(results)[[1]])

plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")

models_to_evaluate <- list( IBCF_cos = list(name = "IBCF", param = list(method = "cosine")), IBCF_cor = list(name = "IBCF", param = list(method = "pearson")), UBCF_cos = list(name = "UBCF", param = list(method = "cosine")), UBCF_cor = list(name = "UBCF", param = list(method = "pearson")), random = list(name = "RANDOM", param=NULL) )

n_recommendations <- c(1, 5, seq(10, 100, 10))

list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n
                         = n_recommendations)

plot(list_results, annotate = 1, legend = "topleft")+ title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")+
title("Precision-recall")

vector_k <- c(5, 10, 20, 30, 40)
models_to_evaluate <- lapply(vector_k, function(k){
  list(name = "IBCF", param = list(method = "cosine", k = k))
})
names(models_to_evaluate) <- paste0("IBCF_k_", vector_k)
n_recommendations <- c(1, 5, seq(10, 100, 10))
list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n
                         = n_recommendations)

plot(list_results, annotate = 1, legend = "topleft")+ title("ROC
curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")+
title("Precision-recall")




# Application of ML
#==================================
cran <- getOption("repos")
cran["dmlc"] <- "https://s3.amazonaws.com/mxnet-r/"
options(repos = cran)
install.packages("mxnet")


install.packages("jpeg")
install.packages("png")

library(devtools)
devtools::install_github('rich-iannone/DiagrammeR')
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")

library(mxnet)
library(EBImage)
library(jpeg)
library(png)



if (!file.exists("synset.txt")) {
  download.file("http://data.dmlc.ml/mxnet/models/imagenet/inception-bn.tar.gz", destfile = "inception-bn.tar.gz")
  untar("inception-bn.tar.gz")
}

model <<- mx.model.load("./Inception-BN", iteration = 126)

synsets <<- readLines("synset.txt")


preproc.image <- function(im) {
  
  # resize to 224 x 224, needed by input of the model.
  resized <- resize(im, 224, 224)
  # convert to array (x, y, channel)
  arr <- as.array(resized) * 255
  dim(arr) <- c(224, 224, 3)
  # substract the mean
  normed <- arr - 117
  # Reshape to format needed by mxnet (width, height, channel, num)
  dim(normed) <- c(224, 224, 3, 1)
  return(normed)
}

#
im <- readImage("~/Desktop/elephant.jpg")
display(im)
normed <- preproc.image(im)
prob <- predict(model, X = normed)
max.idx <- order(prob[,1], decreasing = TRUE)[1:5]
result <- synsets[max.idx]
result

#Credit Card Fraud Detection
#==================================
install.packages("caret")
install.packages("pROC")
install.packages("DMwR")
install.packages("caTools")

library(caret)
library(pROC)
library(DMwR)
library(caTools)
set.seed(1234)

creditCardData <- read.csv("creditcard.csv")

creditCardData$Class<-factor(ifelse(creditCardData$Class==0,"0","1"))

table(data$Class)

splitIndex <- createDataPartition(creditCardData$Class, p = .70,
                                  list = FALSE,
                         
                                           times = 1)

trainSplit <- creditCardData[ splitIndex,]
table(trainSplit$Class)
testSplit <- creditCardData[-splitIndex,]
table(testSplit$Class)

trainSplit <- SMOTE(Class ~ ., trainSplit, perc.over = 200, perc.under=100)
table(trainSplit$Class)

trCtrl <- trainControl(method = "cv", number = 10)

trainSplit$Class<- as.numeric(trainSplit$Class)
tbmodel <- train(Class ~ ., data = trainSplit, method = "treebag",
                 trControl = trCtrl)
tbmodel

trainSplit$Class<-as.factor(trainSplit$Class)

predictors <- names(trainSplit)[names(trainSplit) != 'Class']
pred <- predict(tbmodel$finalModel, testSplit[,predictors])

auc <- roc(testSplit$Class, pred)
print(auc)

plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC with SMOTE:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

