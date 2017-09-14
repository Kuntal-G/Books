# Code snippets for Chapter 11 of R Data analysis cookbook-2nd Edition
# ===================================================================


#Recipe-1. Exploiting vectorized operations

first.name <- c("John", "Jane", "Tom", "Zach")
last.name <- c("Doe", "Smith", "Glock", "Green")
paste(first.name,last.name)

new.last.name <- c("Dalton")
paste(first.name,new.last.name)

username <- function(first, last) {
        tolower(paste0(last, substr(first,1,1)))
 }
username(first.name,last.name)

auto <- read.csv("auto-mpg.csv")
auto$kmpg <- auto$mpg*1.6

sum(1,2,3,4,5)
mean(1,2,3,4,5)
mean(c(1,2,3,4,5))

#Recipe-2. Processing entire rows and columns as a whole using the apply function

m <- matrix(seq(1,16), 4, 4)
apply(m, 1, min)
apply(m, 2, max)
apply(m,c(1,2),function(x) x^2)
apply(m, 1, quantile, probs=c(.4,.8))

array.3d <- array( seq(100,69), dim = c(4,4,2))
array.3d

apply(array.3d, 3, sum)
sum(85:100)

apply(array.3d,c(1,2),sum)

#Recipe-3. Applying a function to elements of a collection with lapply and sapply

auto <- read.csv("auto-mpg.csv", stringsAsFactors=FALSE)

lapply(c(1,2,3), sqrt)

x <- list(a = 1:10, b = c(1,10,100,1000), c=seq(5,50,by=5))
lapply(x,mean)
class(lapply(x,mean))
sapply(x,mean)
class(sapply(x,mean))
sapply(auto[,2:8], min)

sapply(auto[,2:6], summary)
sapply(auto[,2:6], range)

sapply(auto[,2:6], min)
sapply(auto[,2], min)
sapply(as.data.frame(auto[,2]), min)
sapply(as.data.frame(auto[,2]), min, simplify=F)

#Recipe-4. Applying functions to subsets of a vector

auto <- read.csv("auto-mpg.csv", stringsAsFactors=FALSE)
auto$cylinders <- factor(auto$cylinders, levels = c(3,4,5,6,8), labels = c("3cyl","4cyl", "5cyl", "6cyl", "8cyl"))

tapply(auto$mpg,auto$cylinders,mean)
tapply(auto$mpg,list(cyl=auto$cylinders),mean)

by(auto,auto$cylinders,function(x) cor(x$mpg, x$weight))

#Recipe-5. Using the split-apply-combine strategy with plyr

auto <- read.csv("auto-mpg.csv", stringsAsFactors=FALSE)
auto$cylinders <- factor(auto$cylinders, levels = c(3,4,5,6,8), labels = c("3cyl", "4cyl", "5cyl", "6cyl", "8cyl"))

install.packages("plyr")
library(plyr)

ddply(auto, "cylinders", function(df) mean(df$mpg))
ddply(auto, ~ cylinders, function(df) mean(df$mpg))

ddply(auto, c("cylinders","model_year"), function(df) c(mean=mean(df$mpg), min=min(df$mpg), max=max(df$mpg)))

ddply(auto, ~ cylinders + model_year, function(df) c(mean=mean(df$mpg), min=min(df$mpg), max=max(df$mpg)))

auto <- ddply(auto, .(cylinders), transform, mpg.deviation = round(mpg - mean(mpg),2))
auto <- ddply(auto, .(cylinders), mutate, mpg.deviation = round(mpg - mean(mpg),2))

ddply(auto, .(cylinders), summarize, freq=length(cylinders), meanmpg=mean(mpg))

par(mfrow = c(1,2))
d_ply(auto,"cylinders",summarise,
    hist(mpg,xlab="Miles per Gallon",main="Histogram of Miles per
     Gallon",breaks=5))

autos <- list(auto, auto)
big.df <- ldply(autos,I)


subsetData <- select(auto, mpg, horsepower)
head(subsetData)

filter(auto,model_year>80)

mutate(auto, mpg.deviation = round(mpg - mean(mpg),2))

summarise(group_by(auto,cylinders),mean(mpg))

auto %>%
    filter(model_year<80) %>%
    group_by(cylinders) %>%
    summarise(mean(mpg))


#Recipe-6. Slicing, dicing and combining data with data tables

auto <- read.csv("auto-mpg.csv", stringsAsFactors=FALSE)
auto$cylinders <- factor(auto$cylinders, levels = c(3,4,5,6,8), labels = c("3cyl", "4cyl", "5cyl", "6cyl", "8cyl"))

install.packages("data.table")
library(data.table)
autoDT <- data.table(auto)

autoDT[,.(mpg)] #selecting single column
autoDT[,.(mpg,horsepower,cylinders)] #selecting multiple column

autoDT[cylinders %in% c("3cyl","4cyl")]
autoDT[cylinders=="3cyl" & horsepower>90] #Filtering based on multiple condition
autoDT[car_name %like% "chevrolet"] #Like operator for filtering

autoDT[, mean(mpg), by=cylinders]
autoDT[, meanmpg := mean(mpg), by=cylinders]
autoDT[1:5,c(1:3,9:10), with=FALSE]

autoDT[, c("mean_mpg","mpg_deviation"):=list(mean(mpg),round(mpg - mean(mpg),2)), by=cylinders]
autoDT[1:5,c(1:3,10:12), with=FALSE]

autoDT[,lapply(.SD, mean), .SDcols = c("mpg", "horsepower")]


setkey(autoDT,cylinders)
tables()

autoDT["4cyl",c(1:3,9:10),with=FALSE]
autoDT[.("4cyl"),c(1:3,9:10),with=FALSE]

autoDT[, list(meanmpg=mean(mpg), minmpg=min(mpg), maxmpg=max(mpg)), by=cylinders]

autoDT[,c("medianmpg","sdmpg") := list(median(mpg),sd(mpg)), by=cylinders]
autoDT[1:5,c(3,9:12), with=FALSE]

autoDT[,.N ,by=cylinders]
autoDT["4cyl",.N]

autoDT[,medianmpg:=NULL]

emp <- read.csv("employees.csv", stringsAsFactors=FALSE)
dept <- read.csv("departments-1.csv", stringsAsFactors=FALSE)
empDT <- data.table(emp)
deptDT <- data.table(dept)
setkey(empDT,"DeptId")
combine <- empDT[deptDT]
combine[,.N]

dept <- read.csv("departments-2.csv", stringsAsFactors=FALSE)
deptDT <- data.table(dept)
# The following line gives an error
combine <- empDT[deptDT]
# Avoid the error using allow.cartesian=TRUE
combine <- empDT[deptDT, allow.cartesian=TRUE]
combine[,.N]

mash <- empDT[deptDT, nomatch=0]
mash[,.N]

merge(empDT,deptDT, by="DeptId") #Inner join
merge(empDT,deptDT, by="DeptId",all.x = TRUE) #Left join
merge(empDT,deptDT, by="DeptId",all.y = TRUE) #Right join
merge(empDT,deptDT, by="DeptId", all=TRUE) #Full join

empDT[deptDT, max(.SD), by=.EACHI, .SDcols="Salary"]
empDT[,.(AvgSalary = lapply(.SD,mean)), by="DeptId",.SDcols="Salary"]
empDT[deptDT,list(DeptName, AvgSalary = lapply(.SD,mean)),by=.EACHI,.SDcols="Salary"]
