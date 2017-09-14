# Code snippets for Chapter 13 of R Data analysis cookbook-2nd Edition
# ===================================================================


# Recipe: Create standard data summaries
# ---------------------------------------
auto  <- read.csv("auto-mpg.csv",
   header = TRUE, stringsAsFactors = FALSE)

auto$cylinders <- factor(auto$cylinders, levels = c(3,4,5,6,8),
  labels = c("3cyl", "4cyl", "5cyl", "6cyl", "8cyl"))

summary(auto)

str(auto)

summary(auto$cylinders)
summary(auto$mpg)
str(auto$cylinders)

install.packages(c("modeest","raster","moments"))
library(modeest) #For mfv()
library(raster)  #For quantile() and cv()
library(moments) #For skewness() and kurtosis()

mean(auto$mpg)
median(auto$mpg)
mfv(auto$mpg)
quantile(auto$mpg)

sd(auto$mpg)
var(auto$mpg)
cv(auto$mpg)

skewness(auto$mpg)
kurtosis(auto$mpg)

# Recipe: Extract a subset of a data set
# ----------------------------------------
auto <- read.csv("auto-mpg.csv",  stringsAsFactors=FALSE)

auto[1:3, 8:9]
auto[1:3, c(8,9)]

auto[1:3,c("model_year", "car_name")]

auto[auto$mpg == max(auto$mpg) | auto$mpg == min(auto$mpg),]

auto[auto$mpg>30 & auto$cylinders==6, c("car_name","mpg")]

auto[auto$mpg >30 & auto$cyl==6, c("car_name","mpg")]

subset(auto, mpg > 30 & cylinders == 6,  select=c("car_name","mpg"))

auto[auto$mpg > 30]

auto[auto$mpg > 30, ]

auto[,c(-1,-9)]
auto[,-c(1,9)]

auto[, !names(auto) %in% c("No", "car_name")]

auto[auto$mpg %in% c(15,20),c("car_name","mpg")]

auto[1:2,c(FALSE,FALSE,TRUE)]

# Recipe: Spilitting a data set
# ——————————————————-

auto <- read.csv("auto-mpg.csv", stringsAsFactors=FALSE)

carslist <- split(auto, auto$cylinders)

str(carslist[1])

names(carslist[[1]])

# Recipe: Creating random data partitions
# ---------------------------------------
install.packages("caret")
library(caret)
bh <- read.csv("BostonHousing.csv")

trg.idx <- createDataPartition(bh$MEDV, p = 0.8, list = FALSE)
trg.part <- bh[trg.idx, ]
val.part <- bh[-trg.idx, ]

trg.idx <- createDataPartition(bh$MEDV, p = 0.7, list = FALSE)
trg.part <- bh[trg.idx, ]
temp <- bh[-trg.idx, ]
val.idx <- createDataPartition(temp$MEDV, p = 0.5, list = FALSE)
val.part <- temp[val.idx, ]
test.part <- temp[-val.idx, ]

bh2 <- read.csv("boston-housing-classification.csv")
trg.idx <- createDataPartition(bh2$MEDV_CAT, p = 0.7, list = FALSE)
trg.part <- bh2[trg.idx, ]
val.part <- bh2[-trg.idx, ]

bh3 <- read.csv("boston-housing-classification.csv")
trg.idx <- createDataPartition(bh3$MEDV_CAT, p = 0.7, list = FALSE)
trg.part <- bh3[trg.idx, ]
temp <- bh3[-trg.idx, ]
val.idx <- createDataPartition(temp$MEDV_CAT, p = 0.5, list = FALSE)
val.part <- temp[val.idx, ]
test.part <- temp[-val.idx, ]

rda.cb.partition2 <- function(ds, target.index, prob) {
  library(caret)
  train.idx <- createDataPartition(y=ds[,target.index],
      p = prob, list = FALSE)
  list(train =  ds[train.idx, ], val = ds[-train.idx, ])
}

rda.cb.partition3 <- function(ds,
             target.index, prob.train, prob.val) {
  library(caret)
  train.idx <- createDataPartition(y=ds[,target.index],
          p = prob.train, list = FALSE)
  train <- ds[train.idx, ]
  temp <- ds[-train.idx, ]
  val.idx <- createDataPartition(y=temp[,target.index],
          p = prob.val/(1-prob.train), list = FALSE)
  list(train =  ds[train.idx, ],
          val = temp[val.idx, ], test = temp[-val.idx, ])
}

dat1 <- rda.cb.partition2(bh, 14, 0.8)
dat2 <- rda.cb.partition3(bh, 14, 0.7, 0.15)

sam.idx <- sample(1:nrow(bh), 50, replace = FALSE)

# Recipe: Generating standard plots such as histograms, boxplots and scatterplots
# ---------------------------------------------------------------------
auto <- read.csv("auto-mpg.csv")

auto$cylinders <- factor(auto$cylinders, levels = c(3,4,5,6,8),
    labels = c("3cyl", "4cyl", "5cyl", "6cyl", "8cyl"))
attach(auto)

hist(acceleration)

hist(acceleration, col="blue", xlab = "acceleration",
    main = "Histogram of acceleration", breaks = 15)

boxplot(mpg, xlab = "Miles per gallon")

boxplot(mpg ~ model_year, xlab = "Miles per gallon")

boxplot(mpg ~ cylinders)

plot(mpg ~ horsepower)

pairs(~mpg+displacement+horsepower+weight)

hist(mpg, col = rainbow(12))

hist(mpg, prob=TRUE)
lines(density(mpg))


plot(mpg ~ horsepower)
reg <- lm(mpg ~ horsepower)
abline(reg)

plot(mpg ~ horsepower, type = "n")

with(subset(auto, cylinders == "8cyl"), points(horsepower, mpg, col = "blue"))
with(subset(auto, cylinders == "6cyl"),  points(horsepower, mpg, col = "red"))
with(subset(auto, cylinders == "5cyl"),  points(horsepower, mpg, col = "yellow"))
with(subset(auto, cylinders == "4cyl"),  points(horsepower, mpg, col = "green"))
with(subset(auto, cylinders == "3cyl"),  points(horsepower, mpg))

# Recipe: Generate multiple plots on a grid
# ------------------------------------------
auto <- read.csv("auto-mpg.csv")

cylinders <- factor(cylinders, levels = c(3,4,5,6,8),
  labels = c("3cyl", "4cyl", "5cyl", "6cyl", "8cyl"))
attach(auto)


old.par = par()
par(mfrow = c(1,2))
with(auto, {
  plot(mpg ~ weight, main = "Weight vs. mpg")
  plot(mpg ~ acceleration,
  main = "Acceleration vs. mpg")
}
)
par(old.par)

# Recipe: Create plots with the lattice package
# ---------------------------------------------
auto <- read.csv("auto-mpg.csv", stringsAsFactors=FALSE)
cyl.factor <- factor(auto$cylinders,labels=c("3cyl","4cyl", "5cyl","6cyl","8cyl"))

library(lattice)


bwplot(~auto$mpg|cyl.factor, main="MPG by Number of Cylinders",xlab="Miles per Gallon")

xyplot(mpg~weight|cyl.factor, data=auto,
  main="Scatterplots by Cylinders",
  ylab="Miles per Gallon", xlab="Car Weight")

trellis.par.set(theme = col.whitebg())

bwplot(~mpg|cyl.factor, data=auto,main="MPG by Number Of Cylinders",
  xlab="Miles per Gallon",layout=c(2,3),aspect=1)


# Recipe: Creating charts that facilitate comparisons
# --------------------------------------------------

 library(dplyr)
 library(beanplot)

bike <- read.csv("daily-bike-rentals.csv")
 bike$season <- factor(bike$season, levels = c(1,2,3,4),labels = c("Spring", "Summer", "Fall", "Winter"))
bike$workingday <- factor(bike$workingday, levels = c(0,1),labels = c("Work day", "Free day"))
 bike$weathersit <- factor(bike$weathersit, levels = c(1,2,3),labels = c("Clear", "Misty/cloudy", "Light snow"))
 attach(bike)

bike.sum =bike %>%
group_by(season, workingday) %>%
summarize(rental = sum(cnt))


par(mfrow = c(2,2))
spring <- subset(bike, season == "Spring")$cnt
 summer <- subset(bike, season == "Summer")$cnt
 fall <- subset(bike, season == "Fall")$cnt
 winter <- subset(bike, season == "Winter")$cnt

 hist(spring, prob=TRUE, xlab = "Spring daily rentals", main = "")
 lines(density(spring))
 abline(v = mean(spring), col = "red")
 abline(v = median(spring), col = "blue")

hist(summer, prob=TRUE, xlab = "Summer daily rentals", main = "")
 lines(density(summer))
 abline(v = mean(summer), col = "red")
 abline(v = median(summer), col = "blue")

 hist(fall, prob=TRUE, xlab = "Fall daily rentals", main = "")
lines(density(fall))
abline(v = mean(fall), col = "red")
abline(v = median(fall), col = "blue")

hist(winter, prob=TRUE, xlab = "Winter daily rentals", main = "")
 lines(density(winter))
 abline(v = mean(winter), col = "red")
abline(v = median(winter), col = "blue")


 beanplot(bike$cnt ~ bike$season, col = c("blue", "red", "yellow"))


 # Recipe: Creating charts that help to visualize possible causality
 # --------------------------------
 library(lattice)
 bike <- read.csv("daily-bike-rentals.csv")
 bike$season <- factor(bike$season, levels = c(1,2,3,4),
    labels = c("Spring", "Summer", "Fall", "Winter"))
 bike$weathersit <- factor(bike$weathersit, levels = c(1,2,3),
    labels = c("Clear", "Misty/cloudy", "Light snow"))
 bike$dteday = as.Date(bike$dteday, format = "%Y-%m-%d")
attach(bike)

 
 
 bwplot(cnt ~ weathersit, data=bike, layout=c(1,1),xlab = "Weathersit", ylab = "Frequency",
        par.settings = list(box.rectangle = list(fill= rep(c('red','yellow','green'),2))))
 
 bwplot(cnt ~ weathersit,xlab = "Weathersit", ylab = "Frequency",panel=function(x,y,...){
   panel.bwplot(x,y,...)
   panel.stripplot(x,y,jitter.data = TRUE,...)
 }, par.settings = list(box.rectangle = list(fill= rep(c('red','yellow','green'),2))))
 
 
 

