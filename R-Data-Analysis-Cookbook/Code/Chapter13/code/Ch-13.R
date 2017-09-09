# Using Java objects in R

install.packages(‘rJava’)
library(rJava)
.jinit()
.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")

.jaddClassPath(getwd())
.jclassPath()

s <- .jnew("java/lang/String", "Hello World!")
print(s)
.jstrVal(s)
.jcall(s,"S","toLowerCase")
.jcall(s,"S","replaceAll","World","SV")


javaVector <- .jnew("java/util/Vector")
months <- month.abb
sapply(months, javaVector$add)
javaVector$size()
javaVector$toString()

monthsArray <- .jarray(month.abb)
yearsArray <- .jarray(as.numeric(2010:2015))
calArray <- .jarray(list(monthsArray,yearsArray))
print(monthsArray)
.jevalArray(monthsArray)
print(l <- .jevalArray(calArray))
lapply(l, .jevalArray)

hw <- .jnew("javasamples.HelloWorld")
hello <- .jcall(hw,"S", "getString")
hello

greet <- .jnew("javasamples.Greeting")
print(greet)
g <- .jcall(greet,"S", "getString", "Kuntal")
print(g)
.jstrVal(g)

jvm = .jnew("java.lang.System")
jvm.props = jvm$getProperties()$toString()
jvm.props <- strsplit(gsub("\\{(.*)}", "\\1", jvm.props), ", ")[[1]]
jvm.props

.jmethods(s,"trim")
.jmethods(s)

export R_HOME=/Library/Frameworks/R.framework/Resources
export PATH=$PATH:/Library/Frameworks/R.framework/Resources/bin/

# Using JRI to call R functions from Java

From javasamples directory::

javac -cp .:../lib/* *java

java  -Djava.library.path=/Library/Frameworks/R.framework/Resources/library/rJava/jri -cp ..:../lib/* javasamples.SimpleJRIStat

java -Djava.library.path=/Library/Frameworks/R.framework/Resources/library/rJava/jri/ -cp ..:../lib/* javasamples.SimplePlot /Users/sv/book/Chapter11

# Using Rserv to call R functions from Java

install.packages(“Rserve”)
library(Rserve)
Rserve(args="--no-save")  — on Mac, Rserve() on windows

# in the following command, be sure to change the last command argument to point
# to where you have the auto-mpg.csv file
java -cp ..:../lib/* javasamples.SimpleGGPlot /Users/sv/book/Chapter11

java -cp ..:../lib/* javasamples.SimpleRservStat

# Executing R script from Java

java -Djava.library.path=/Library/Frameworks/R.framework/Resources/library/rJava/jri/ -cp ..:../lib/* javasamples.InvokeRScript mpg weight /Users/sv/book/Chapter11

# Using xlsx to connect to Excel

library(xlsx)   —we are not discussing XLConnect package here.
auto <- read.csv("auto-mpg.csv", stringsAsFactors=FALSE)

write.xlsx(auto, file = "auto.xlsx", sheetName = "autobase", row.names = FALSE)

auto$kmpg <- auto$mpg * 1.6
auto$mpg_deviation <- (auto$mpg - mean(auto$mpg))/auto$mpg

auto.wb <- createWorkbook()
sheet1 <- createSheet(auto.wb,"auto1")
rows <- createRow(sheet1, rowIndex=1)
cell.1 <- createCell(rows, colIndex=1)[[1,1]]
setCellValue(cell.1, "Hello Auto Data!")

addDataFrame(auto, sheet1, startRow=3, row.names=FALSE)

cs <- CellStyle(auto.wb) + Font(auto.wb, isBold=TRUE, color="red")
setCellStyle(cell.1, cs)
saveWorkbook(auto.wb,"auto_wb.xlsx")

auto.wb <- loadWorkbook("auto_wb.xlsx")
sheet2 <- createSheet(auto.wb,"auto2")
addDataFrame(auto[,1:9], sheet2, row.names=FALSE)
saveWorkbook(auto.wb, "auto_wb.xlsx")

wb <- loadWorkbook("auto_wb.xlsx")
sheets <- getSheets(wb)
sheet <- sheets[[2]]
addDataFrame(auto[,10:11], sheet, startColumn=10, row.names=FALSE)
saveWorkbook(wb, "newauto.xlsx")

#Recipe: Reading data from relational databases
#----------------------------------------------
customer <- c("John", "Peter", "Jane")
orddt <- as.Date(c('2014-10-1','2014-1-2','2014-7-6'))
ordamt <- c(280, 100.50, 40.25)
order <- data.frame(customer,orddt,ordamt)

library(RODBC)
con <- odbcConnect("order_dsn", uid="user", pwd="pwd")
sqlSave(con,order,"orders",append=FALSE)
custData <- sqlQuery(con, "select * from orders")
close(con)

library(RMySQL)
library(RMySQL)
con <- dbConnect(MySQL(), dbname="recommendation",host="127.0.0.1", port=3306,username="root",password="root")
dbWriteTable(con,"orders", order)
dbReadTable(con,"accomodation")
dbGetQuery(con,"select * from accommodation limit 5")
rs <- dbSendQuery(con, "select location, max(price) from accommodation group by location order by max(price) desc")
while(!dbHasCompleted(rs)) {
   fetch(rs,n=2)
  }

dbClearResult(rs)
dbDisconnect(con)
dbListConnections(dbDriver("MySQL"))


library(RJDBC)
# In the followi g command, be sure to point to where the downloaded jar file resides
driver <- JDBC("com.mysql.jdbc.Driver",
               classpath=
                 "/etc/jdbc/mysql-connector-java-5.1.34-bin.jar", "`")
con <- dbConnect(driver,"jdbc:mysql://host:port/Customer"
                 ,"username","password")
# The remaining operations are identical to RMySQL

fetch(rs,n=-1)

dbSendQuery(con, statement=paste(
  "select ordernumber, orderdate, customername",
  "from orders o, customers c",
  "where o.customer = c.customer",
  "and c.state = 'NJ'",
  "ORDER BY ordernumber"))


# Recipe: Read data from NoSQL databases -- mongoDB
------------------------------
install.packages(c("rmongodb", "mongolite", "ggplot2", "ggmap", "data.table", "dplyr"))
library(data.table)
crimes=data.table::fread("Chicago_Crimes_2012-2017.csv")
 library(rmongodb)
 library(mongolite)

 crime_collection = mongo(collection = "crimes", db = "Chicago")
 crime_collection$insert(crimes)

# Using mongolite
 crime_collection$count()
crime_collection$iterate()$one()

# using rmongodb
 mongo <- mongo.create(host = "localhost")
mongo.is.connected(mongo)
 mongo.get.database.collections(mongo,"Chicago")
 mongo.find.one(mongo, ns="Chicago.crimes")

crime_collection$count('{"PrimaryType" : "ASSAULT" }')

 library(dplyr)

 crimes<-crime_collection$find('{}', fields = '{"_id":0, "PrimaryType":1,"Year":1}')

crimes%>%group_by(PrimaryType)%>%summarize(Count=n())%>%arrange(desc(Count))%>%head(5)

> library(ggplot2)
> crime_collection$aggregate('[{"$group":{"_id":"$LocationDescription", "Count": {"$sum":1}}}]')%>%na.omit()%>%
arrange(desc(Count))%>%head(10)%>%
ggplot(aes(x=reorder(`_id`,Count),y=Count))+
geom_bar(stat="identity",fill='#c37600')+geom_text(aes(label = Count), color = "blue") +coord_flip()+xlab("Location Description")



 library(maps)
library(ggmap)
 query= crime_collection$find('{}', fields = '{"_id":0, "Latitude":1, "Longitude":1,"Year":1}')
 LatLonCounts=as.data.frame(table(round(query$Longitude,2), round(query$Latitude,2)))

 # Convert the Longitude and Latitude variable to numbers:
 LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
 LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))

 ggmap(chicago)+geom_tile(data = LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill="red")+ggtitle("Crime Distribution")+labs(alpha="Count")+theme(plot.title = element_text(color="blue",hjust=0.5))


# Recipe:Working with in-memory data processing with Apache Spark
----------------------------------------------------------------
if (!require('devtools')) install.packages('devtools')
devtools::install_github('apache/spark@v2.1.1', subdir='R/pkg')

 library(SparkR)

 Sys.setenv(SPARK_HOME='/path/to/spark/directory')
.libPaths(c(file.path(Sys.getenv('SPARK_HOME'), 'R', 'lib'), .libPaths()))
sparkR.session()

 irisDF <- createDataFrame(iris) //Create a Spark DataFrame
 showDF(irisDF,4) //Print the contents of the Spark DataFrame


 subIrisDF <- select(irisDF, c("Sepal_Length","Petal_Length"))
 showDF(subIrisDF,5)


 library(magrittr)
 aggrIrishDF <- irisDF %>% groupBy("Species") %>%
avg("Sepal_Length") %>%
withColumnRenamed("avg(Sepal_Length)","avg_sepal_len") %>%
orderBy ("Species")
 #Format the computed double column
aggrIrishDF$avg_sepal_len <- format_number(aggrIrishDF$avg_sepal_len,2)
 #This shows group wise average sepal length sorted by species name.
 showDF(aggrIrishDF)

 #Create a view of the Dataframe to run SQL over the data
 createOrReplaceTempView(irisDF,"iris_vw")
 collect(sql("SELECT * FROM iris_tbl LIMIT 5"))
collect(sql("SELECT Species, avg(Sepal_Length)
avg_sepal_length, avg(Sepal_Width) avg_sepal_width FROM iris_tbl
GROUP BY Species ORDER BY avg_sepal_length desc")

#Classification with SparkR
 df <- read.df("boston-housing-logistic.csv", "csv", header = "true", inferSchema = "true", na.strings = "NA")
 traindata <- sample(df,FALSE,0.8)
 testdata <- except(df,traindata)

 model <- glm(CLASS ~ NOX+DIS+RAD+TAX+PTRATIO,data = traindata, family = "binomial")
 predictions <- predict(model, newData = testdata)
 head(predictions)


#Movie Lens Recommendation System with SparkR
 jdbcUrl="jdbc:mysql://localhost:3306/recommendation"
 dfRates = read.jdbc(jdbcUrl, "recommendation.rating", user = "root", password = "root")
df_list <- randomSplit(dfRates, c(7,3), 2)
 recommendDF <- df_list[[1]]
 recommendTestDF <- df_list[[2]]

# Fit a recommendation model using ALS with spark.als
 model <- spark.als(recommendDF, maxIter = 5, regParam = 0.01, userCol = "userId",itemCol = "movieId", ratingCol = "rating")

summary(model) # Model summary
predictions <- predict(model, recommendTestDF)
head(predictions)
