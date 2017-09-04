#Code snippets for Chapter 6
#===========================

#Recipe: Exploring Finance data
#--------------------------------

AMZN=read.csv("AMZN.csv",stringsAsFactors = FALSE)
str(AMZN)
GOOG=read.csv("GOOG.csv",stringsAsFactors = FALSE)
head(GOOG, 5)

AMZN$Date=as.Date(AMZN$Date)
GOOG$Date=as.Date(GOOG$Date)

library(ggplot2)
ggplot(AMZN,aes(Date,Close)) + 
  geom_line(aes(color="amazon")) +
  geom_line(data=GOOG,aes(color="google")) +
  labs(color="Legend") +
  scale_colour_manual("", breaks = c("amazon","google"),
                      values = c("blue", "brown")) +
  ggtitle("Closing Stock Prices: Amazon and Google ") + 
  theme(plot.title = element_text(lineheight=.7, face="bold"))


install.packages("quantmod")
library("quantmod")
getSymbols('AMZN')
barChart(AMZN)
chartSeries(AMZN, TA=NULL)

data=AMZN[,4]
chartSeries(data, TA='addMACD()')

#Recipe: Creating and examining date objects
#--------------------------------
Sys.Date()

as.Date("1/1/80", format = "%m/%d/%y")

as.Date("1/1/1980", format = "%m/%d/%Y")

as.Date("1970/1/1")

as.Date("70/1/1")

dt <- as.Date("1-1-70", format = "%m-%d-%y")

as.numeric(dt)  

as.Date("Jan 15, 2015", format = "%b %d, %Y")

as.Date("January 15, 15", format = "%B %d, %y")

dt <- 1000
class(dt) <- "Date"
dt                 

dt <- -1000
class(dt) <- "Date"
dt                 

as.Date(1000, origin = as.Date("1980-03-31"))

as.Date(-1000, origin = as.Date("1980-03-31"))

dt <- as.Date(1000, origin = as.Date("1980-03-31"))
dt

format(dt, "%Y")

as.numeric(format(dt, "%Y"))

format(dt, "%y")

format(dt, "%m")

as.numeric(format(dt, "%m"))

format(dt, "%b")

format(dt, "%B")

months(dt)

weekdays(dt)

quarters(dt)

julian(dt)

julian(dt, origin = as.Date("1980-03-31"))

#Recipe: Operating on date objects
#----------------------------------
dt <- as.Date("1/1/2001", format = "%m/%d/%Y")
dt

dt + 100                 

dt + 31

dt1 <- as.Date("1/1/2001", format = "%m/%d/%Y")
dt2 <- as.Date("2/1/2001", format = "%m/%d/%Y")
dt1-dt1

dt2-dt1

dt1-dt2

dt2 > dt1

dt2 == dt1

d1 <- as.Date("1980/1/1")
d2 <- as.Date("1982/1/1")
seq(d1, d2, "month")

d3 <- as.Date("1980/1/5")
seq(d1, d3, "day")


seq(d1, d2, "2 months")

seq(from = d1, by = "4 months", length.out = 4 )

seq(from = d1, by = "3 weeks", length.out = 2)[2]

#Recipe: Perform preliminary analyses on time series data
#--------------------------------------------------------
wm <- read.csv("walmart.csv")

plot(wm$Adj.Close, type = "l")

d <- diff(wm$Adj.Close)
plot(d, type = "l")


hist(d, prob = TRUE, ylim = c(0,0.8), main = "Walmart stock", col = "blue")
lines(density(d), lwd = 3)

wmm <- read.csv("walmart-monthly.csv")
wmm.ts <- ts(wmm$Adj.Close)
d <- diff(wmm.ts)
wmm.return <- d/lag(wmm.ts, k=-1)
hist(wmm.return, prob = TRUE, col="blue")
diff(wmm$Adj.Close, lag = 2)

#Recipe: Using time series objects
#---------------------------------------------------
s <- read.csv("ts-example.csv")

s.ts <- ts(s)
class(s.ts)


plot(s.ts)

s.ts.a <- ts(s, start = 2002)
s.ts.a

plot(s.ts.a)

s.ts.m <- ts(s, start = c(2002,1), frequency = 12)
s.ts.m
plot(s.ts.m)  

s.ts.q <- ts(s, start = 2002, frequency = 4)
s.ts.q
plot(s.ts.q)

start(s.ts.m)

end(s.ts.m)

frequency(s.ts.m)

prices <- read.csv("prices.csv")
prices.ts <- ts(prices, start=c(1980,1), frequency = 12)

plot(prices.ts)

plot(prices.ts, plot.type = "single", col = 1:2)
legend("topleft", colnames(prices.ts), col = 1:2, lty = 1)

#Recipe: Decomposing time series
#-----------------------------
prices <- read.csv("prices.csv")

prices.ts = ts(prices, start = c(1980,1), freq = 12)
plot(prices.ts[,2])

prices.stl <- stl(log(prices.ts[,1]), s.window = "period")

plot(prices.stl)

prices.dec <- decompose(log(prices.ts[,2]))
plot(prices.dec)

gas.seasonally.adjusted <- prices.ts[,2] - prices.dec$seasonal 
plot(gas.seasonally.adjusted)

#Recipe: Filtering time series data
#---------------------------------
s <- read.csv("ts-example.csv")

n <- 7
wts <- rep(1/n, n)

s.filter1 <- filter(s$sales, filter = wts, sides = 2)
s.filter2 <- filter(s$sales, filter = wts, sides = 1)

plot(s$sales, type = "l")
lines(s.filter1, col = "blue", lwd = 3)
lines(s.filter2, col = "red", lwd = 3)

#Recipe: Smoothing and forecasting using the Holt-Winters method
#---------------------------------------------------------
infy <- read.csv("infy-monthly.csv")

infy.ts <- ts(infy$Adj.Close, start = c(1999,3), freq = 12)

infy.hw <- HoltWinters(infy.ts)


plot(infy.hw, col = "blue", col.predicted = "red")

infy.hw$SSE

infy.hw$alpha

infy.hw$beta

infy.hw$gamma

head(infy.hw$fitted)

library(forecast)
infy.forecast <- forecast(infy.hw, h=20)
plot(infy.forecast)

#Recipe: Building an automated ARIMA model
#--------------------------------------
infy <- read.csv("infy-monthly.csv")

infy.ts <- ts(infy$Adj.Close, start = c(1999,3), freq = 12)

infy.arima <- auto.arima(infy.ts)

summary(infy.arima)

infy.forecast <- forecast(infy.arima, h=10)

plot(infy.forecast)






