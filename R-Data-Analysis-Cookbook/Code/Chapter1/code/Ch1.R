# Code snippets for Chapter 1 of R Data analysis cookbook-2nd Edition
# ===================================================================

# Recipe: Acquire and prepare your ingredients -- Your Data

auto <- read.csv("auto-mpg.csv", header=TRUE, sep = ",")
names(auto)
auto  <- read.csv("auto-mpg-noheader.csv", header=FALSE)
head(auto,2)
auto  <- read.csv("auto-mpg-noheader.csv")
head(auto,2)
auto <- read.csv("auto-mpg-noheader.csv", header=FALSE, col.names = c("No", "mpg", "cyl", "dis","hp", "wt", "acc", "year", "car_name"))
head(auto,2)
auto  <- read.csv("auto-mpg.csv", na.strings="")
auto <- read.csv("auto-mpg.csv",stringsAsFactors=FALSE)
dat <- read.csv("http://www.exploredata.net/ftp/WHO.csv")

# Recipe: Reading XML Data

install.packages("XML")
library(XML)

url <- "cd_catalog.xml"

xmldoc <- xmlParse(url)
rootNode <- xmlRoot(xmldoc)
rootNode[1]
data <- xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))
cd.catalog <- data.frame(t(data),row.names=NULL)
cd.catalog[1:2,]

url <- "WorldPopulation-wiki.htm"

tables <- readHTMLTable(url)
world.pop <- tables[[6]]
world.pop
table <- readHTMLTable(url,which=6)
table
# Recipe: Reading JSON data

if (!require("jsonlite")) install.packages("jsonlite")

library(jsonlite)

dat.1 <- fromJSON("students.json")
dat.2 <- fromJSON("student-courses.json")

url <- "http://finance.yahoo.com/webservice/v1/symbols/allcurrencies/quote?format=json"
jsonDoc <- fromJSON(url)

dat <- jsonDoc$list$resources$resource$fields

dat[1:2,]
dat.1[1:3,]
dat.2[,c(1,2,4:5)]

# Recipe: Reading data from fixed-width formatted files
student  <- read.fwf("student-fwf.txt", widths=c(4,15,20,15,4), col.names=c("id","name","email","major","year"))
student  <- read.fwf("student-fwf-header.txt", widths=c(4,15,20,15,4), header=TRUE, sep="\t",skip=2)

# Recipe: Reading data from R files and R libraries

customer <- c("John", "Peter", "Jane")
orderdate <- as.Date(c('2014-10-1','2014-1-2','2014-7-6'))
orderamount <- c(280, 100.50, 40.25)
order <- data.frame(customer,orderdate,orderamount)
names <- c("John", "Joan")
save(order, names, file="test.Rdata")
saveRDS(order,file="order.rds")
remove(order)

load("test.Rdata")
ord <- readRDS("order.rds")

data(iris)
data(list = c("cars","iris"))

save.image(file = "all.RData")

odd <- c(1,3,5,7)
even <- c(2,4,6,8)
save(list=c("odd","even"),file="OddEven.Rdata")

attach("order.Rdata")
data()

# Recipe: Removing cases with missing values
dat <- read.csv("missing-data.csv", na.strings="")

dat.cleaned <- na.omit(dat)

is.na(dat[4,2])
is.na(dat$Income)

dat.income.cleaned <- dat[!is.na(dat$Income),]
nrow(dat.income.cleaned)

complete.cases(dat)

dat.cleaned <- dat[complete.cases(dat),]
nrow(dat.cleaned)

dat$Income[dat$Income==0] <- NA

mean(dat$Income)
mean(dat$Income, na.rm = TRUE)

# Recipe: Replacing missing values with the mean
dat <- read.csv("missing-data.csv", na.strings = "")
dat$Income.imp.mean <- ifelse(is.na(dat$Income),
                              mean(dat$Income, na.rm=TRUE), dat$Income)

rand.impute <- function(a) {
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}

random.impute.data.frame <- function(dat, cols) {
  nms <- names(dat)
  for(col in cols) {
    name <- paste(nms[col],".imputed", sep = "")
    dat[name] <- rand.impute(dat[,col])
  }
  dat 
}

dat <- read.csv("missing-data.csv", na.strings="")
random.impute.data.frame(dat, c(1,2))

# Recipe: Removing duplicate cases
salary <- c(20000, 30000, 25000, 40000, 30000, 34000, 30000)
family.size <- c(4,3,2,2,3,4,3)
car <- c("Luxury", "Compact", "Midsize", "Luxury",
         "Compact", "Compact", "Compact")
prospect <- data.frame(salary, family.size, car)

prospect.cleaned <- unique(prospect)
nrow(prospect)
nrow(prospect.cleaned)

duplicated(prospect)

prospect[duplicated(prospect), ]

# Recipe: Rescaling variables to [0, 1]
install.packages("scales")
library(scales)
students <- read.csv("data-conversion.csv")

students$Income.rescaled <- rescale(students$Income)

rescale(students$Income)
(students$Income - min(students$Income)) /
  (max(students$Income) - min(students$Income))

rescale(students$Income, to = c(1, 100))

rescale.many <- function(dat, column.nos) {
  nms <- names(dat)
  for(col in column.nos) {
    name <- paste(nms[col],".rescaled", sep = "")
    dat[name] <- rescale(dat[,col])
  }
  cat(paste("Rescaled ", length(column.nos),
            " variable(s)\n"))
  dat 
}

rescale.many(students, c(1,4))

# Recipe: Normalizing or standardizing data in a data frame
housing <- read.csv("BostonHousing.csv")
housing.z <- scale(housing)

scale.many <- function(dat, column.nos) {
  nms <- names(dat)
  for(col in column.nos) {
    name <- paste(nms[col],".z", sep = "")
    dat[name] <- scale(dat[,col])
  }
  cat(paste("Scaled ", length(column.nos), " variable(s)\n"))
  dat 
}

housing <- read.csv("BostonHousing.csv")
housing <- scale.many(housing, c(1,3,5:7))

names(housing)

# Recipe: Binning numerical data

students <- read.csv("data-conversion.csv")
b <- c(-Inf, 10000, 31000, Inf)

names <- c("Low", "Medium", "High")

students$Income.cat <- cut(students$Income, breaks = b, labels = names)
students

b <- c(-Inf, 10000, 31000, Inf)
students$Income.cat1 <- cut(students$Income, breaks = b)
students

students$Income.cat2 <- cut(students$Income,
                            breaks = 4, labels = c("Level1", "Level2",
                                                   "Level3","Level4"))

# Recipe: Creating dummies for catefgorical variables

install.packages("dummies")
library(dummies)
students <- read.csv("data-conversion.csv")

students.new <- dummy.data.frame(students, sep = ".")
names(students.new)

dummy(students$State, sep = ".")

students.new1 <- dummy.data.frame(students,
                                  names = c("State","Gender") , sep = ".")


# Recipe: Handling missing data

install.packages("Hmisc")
housing.dat <- read.csv("housing-with-missing-value.csv",header = TRUE, stringsAsFactors = FALSE)
summary(housing.dat)

housing.dat.1 <- na.omit(housing.dat)

drop_na <- c("rad")
housing.dat.2 <-housing.dat [complete.cases(housing.dat [ , !(names(housing.dat)) %in% drop_na]),]

summary(housing.dat.1$rad)
summary(housing.dat.1$ptratio)
summary(housing.dat.2$rad)
summary(housing.dat.2$ptratio)

housing.dat.3 <- housing.dat$rad <- NULL

drops <- c("ptratio","rad")
housing.dat.4 <- housing.dat[ , !(names(housing.dat) %in% drops)]
summary(housing.dat.4)

library(Hmisc)
#replace with mean
housing.dat$ptratio <- impute(housing.dat$ptratio, mean)
housing.dat$rad <- impute(housing.dat$rad, mean)
#replace with median
housing.dat$ptratio <- impute(housing.dat$ptratio, median)
housing.dat$rad <- impute(housing.dat$rad, median) 
#replace with mode/constant value
housing.dat$ptratio <- impute(housing.dat$ptratio, 18)
housing.dat$rad <- impute(housing.dat$rad, 6)

summary(housing.dat)

#---- Visualize missing information

install.packages("mice")
library(mice)
md.pattern(housing.dat)

install.packages("VIM")
library(VIM)
aggr_plot <- aggr(housing.dat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(housing.dat), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Recipe: Correcting data

install.packages("tidyr")
library(tidyr)
crimeData <- read.csv("USArrests.csv",stringsAsFactors = FALSE)
View(crimeData)

crimeData <- cbind(state = rownames(crimeData), crimeData)

#Gathering 
crimeData.1 <- gather(crimeData,
                          key = "crime_type",
                          value = "arrest_estimate",
                          Murder:UrbanPop)

crimeData.1

crimeData.2 <- gather(crimeData,
                      key = "crime_type",
                      value = "arrest_estimate",
                      -state)

crimeData.2

crimeData.3 <- gather(crimeData,
                      key = "crime_type",
                      value = "arrest_estimate",
                      Murder, Assault)

crimeData.3

# Spreading
crimeData.4 <- spread(crimeData.2,
                      key = "crime_type",
                      value = "arrest_estimate"
)
crimeData.4

# Uniting
crimeData.5 <- unite(crimeData,
                     col = "Murder_Assault",
                     Murder, Assault,
                     sep = "_")
crimeData.5

# Separating
crimeData.6 <- separate_(crimeData.5,
                         col = "Murder_Assault",
                         into = c("Murder", "Assault"),
                         sep = "_")
crimeData.6


# Recipe: Imputing data
install.packages("mice")
library(mice)
housingData <- read.csv("housing-with-missing-value.csv",header = TRUE, stringsAsFactors = FALSE)


#imputing only two columns having missing values
columns=c("ptratio","rad")
imputed_Data <- mice(housingData[,names(housingData) %in% columns], m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

completeData <- complete(imputed_Data)
housingData$ptratio <- completeData$ptratio
housingData$rad <- completeData$rad

anyNA(housingData)

library(Hmisc)
impute_arg <- aregImpute(~ ptratio + rad , data = housingData, n.impute = 5)
impute_arg
impute_arg$imputed$rad



# Recipe: Detecting outliers

ozoneData <- read.csv("ozone.csv", stringsAsFactors=FALSE)
outlier_values <- boxplot.stats(ozoneData$pressure_height)$out

boxplot(ozoneData$pressure_height, main="Pressure Height", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

boxplot(ozone_reading ~ Month, data=ozoneData, main="Ozone reading across months") 

# Treating the outliers with Mean/Median Imputation
impute_outliers <- function(x,removeNA = TRUE){
  quantiles <- quantile( x, c(.05, .95 ),na.rm = removeNA )
  x[ x < quantiles[1] ] <- mean(x,na.rm = removeNA )
  x[ x > quantiles[2] ] <- median(x,na.rm = removeNA )
  x
}

imputed_data <- impute_outliers(ozoneData$pressure_height)

par(mfrow = c(1, 2))
boxplot(ozoneData$pressure_height, main="Pressure Height having Outliers", boxwex=0.3)
boxplot(imputed_data, main="Pressure Height with imputed data", boxwex=0.3)


#Handling extreme values with Capping
replace_outliers <- function(x, removeNA = TRUE) {
  pressure_height <- x
  qnt <- quantile(pressure_height, probs=c(.25, .75), na.rm = removeNA)
  caps <- quantile(pressure_height, probs=c(.05, .95), na.rm = removeNA)
  H <- 1.5 * IQR(pressure_height, na.rm = removeNA)
  pressure_height[pressure_height < (qnt[1] - H)] <- caps[1]
  pressure_height[pressure_height > (qnt[2] + H)] <- caps[2]
  pressure_height
}

capped_pressure_height <- replace_outliers(ozoneData$pressure_height)

par(mfrow = c(1, 2))
boxplot(ozoneData$pressure_height, main="Pressure Height with Outliers", boxwex=0.1)
boxplot(capped_pressure_height, main="Pressure Height without Outliers", boxwex=0.1)

#-- lof outlier detection
install.packages("DMwR")
library(DMwR)
outlier.scores <- lofactor(ozoneData, k=3)
outliers <- order(outlier.scores, decreasing=T)[1:3]
print(outliers)
