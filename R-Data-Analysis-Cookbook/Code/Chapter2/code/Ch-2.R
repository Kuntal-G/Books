# Code snippets for Chapter 2 of R Data analysis cookbook
# ========================================================

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
 library(ggplot2)
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

ggplot(bike.sum, aes(x= season, y= rental))
 + geom_bar(show.legend = TRUE, stat = "identity")
 + labs(title = "Rentals for Season and Day")

ggplot(bike.sum, aes(x= season, y= rental, fill = workingday, label = scales::comma(rental))) + geom_bar(show.legend = TRUE, stat = "identity") + labs(title = "Rentals for Season and Day") + scale_y_continuous(labels = scales::comma) + geom_text(size = 3, position = position_stack(vjust = 0.5)

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
>lines(density(fall))
abline(v = mean(fall), col = "red")
abline(v = median(fall), col = "blue")

hist(winter, prob=TRUE, xlab = "Winter daily rentals", main = "")
 lines(density(winter))
 abline(v = mean(winter), col = "red")
abline(v = median(winter), col = "blue")



qplot(cnt, data = bike) + facet_wrap(~ season, nrow=2) + geom_histogram(fill = "blue") +
 geom_vline(xintercept = mean(cnt), colour = "red")+
 labs(x = "Rentals", y= "Frequency", title = "Rentals Frequency by Season") +
 theme(plot.title = element_text(hjust = 0.5))


 qplot(cnt, data = bike, fill = season) + labs(x = "Rentals", y= "Frequency", title = "Rentals Frequency by Season") + theme(plot.title = element_text(hjust = 0.5))

 ggplot(bike, aes(x = season, y = cnt)) + geom_boxplot()

 qplot(season, cnt, data = bike, geom = c("boxplot"), fill = season) + labs(x = "Season", y= "Frequency", title = "Rentals Frequency by Season") + theme(plot.title = element_text(hjust = 0.5))


 beanplot(bike$cnt ~ bike$season, col = c("blue", "red", "yellow"))


 # Recipe: Creating charts that help to visualize possible causality
 # --------------------------------
 library(ggplot2)
 bike <- read.csv("daily-bike-rentals.csv")
 bike$season <- factor(bike$season, levels = c(1,2,3,4),
    labels = c("Spring", "Summer", "Fall", "Winter"))
 bike$weathersit <- factor(bike$weathersit, levels = c(1,2,3),
    labels = c("Clear", "Misty/cloudy", "Light snow"))
 bike$dteday = as.Date(bike$dteday, format = "%Y-%m-%d")
attach(bike)

 qplot(weathersit, cnt, data = bike, geom = c("boxplot"), fill = weathersit) +
labs(x = "Weathersit", y= "Frequency", title = "Rentals Frequency by Weathersit") +
theme(plot.title = element_text(hjust = 0.5)) +
geom_jitter(shape=1, position=position_jitter(.20))

qplot(weathersit, cnt, data = bike, geom = c("violin"), fill = weathersit) + labs(x = "Weathersit", y= "Frequency", title = "Rentals Frequency by Weathersit") + theme(plot.title = element_text(hjust = 0.5)) + geom_dotplot(binaxis='y', stackdir='center', dotsize=.5)


qplot(weathersit, cnt, data = bike, geom = c("violin"), fill = weathersit) + labs(x = "Weathersit", y= "Frequency", title = "Rentals Frequency by Weathersit") + theme(plot.title = element_text(hjust = 0.5)) + geom_dotplot(binaxis='y', stackdir='center', dotsize=.5)

qplot(weathersit, cnt, data = bike, geom = c("violin"), fill = weathersit) + labs(x = "Weathersit", y= "Frequency", title = "Rentals Frequency by Weathersit") + theme(plot.title = element_text(hjust = 0.5)) + geom_boxplot(width = .3)

ggplot(bike, aes(x = dteday, y = cnt)) + geom_line()+ labs(x = "Date", y= "Rentals", title = "Rentals by Time and Weathersit") + theme(plot.title = element_text(hjust = 0.5))


ggplot(bike, aes(x = dteday, y = cnt, colour = weathersit)) + geom_line()+ labs(x = "Date", y= "Rentals", title = "Rentals by Time and Weathersit") + theme(plot.title = element_text(hjust = 0.5)) + facet_grid(weathersit ~.)


# Recipe: Create multivariate plots
# --------------------------------
library(ggplot2)
library(GGally)
bike <- read.csv("daily-bike-rentals.csv")
bike$season <- factor(bike$season, levels = c(1,2,3,4),
  labels = c("Spring", "Summer", "Fall", "Winter"))
bike$weathersit <- factor(bike$weathersit, levels = c(1,2,3),
  labels = c("Clear", "Misty/cloudy", "Light snow"))
bike$windspeed.fac <- cut(bike$windspeed, breaks=3,
  labels=c("Low", "Medium", "High"))
bike$weekday <- factor(bike$weekday, levels = c(0:6),
  labels = c("Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat"))
attach(bike)

plot <- ggplot(bike,aes(temp,cnt))

plot + geom_point(size=3, aes(color=factor(windspeed))) +
    geom_smooth(method="lm", se=FALSE, col="red") +
    facet_grid(weekday ~ season) + theme(legend.position="bottom")


auto <- read.csv("auto-mpg.csv", stringsAsFactors=FALSE)
auto$cylinders <- factor(auto$cylinders,labels=c("3cyl","4cyl", "5cyl","6cyl","8cyl"))

ggpairs(auto[,2:5])

ggpairs(auto[,2:5], aes(colour = cylinders, alpha = 0.4 ), title = "Multivariate Analysis") + theme(plot.title = element_text(hjust = 0.5))


ggpairs(auto[,2:5], aes(colour = cylinders, alpha = 0.4 ), title = "Multivariate Analysis", upper = list(continuous = "density"), lower = list(combo = "denstrip")) + theme(plot.title = element_text(hjust = 0.5))
