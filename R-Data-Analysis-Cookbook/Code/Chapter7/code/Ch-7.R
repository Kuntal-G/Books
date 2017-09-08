# Creating ScatterPlots
install.packages("ggplot2")
library(ggplot2)
auto <- read.csv("auto-mpg.csv", stringsAsFactors=FALSE)

plot <- ggplot(auto, aes(weight, mpg))
plot + geom_point()

plot + geom_point(alpha=1/2, size=5, aes(color=factor(cylinders))) +
  geom_smooth(method="lm", se=FALSE, col="green") +
  facet_grid(cylinders~.) + 
  labs(x = "Weight") +
  labs(y = "Miles Per Gallon") + 
  labs(title = "MPG Vs Weight")

qplot(weight, mpg, data=auto, geom=c("point", "smooth"), method="lm", formula=y~x, color=cylinders, main="Regression of MPG on Weight")



#Creating Lines graphs
install.packages("ggplot2")
library(ggplot2)
mtcars <- read.csv("mtcars.csv", stringsAsFactors=FALSE)

plot <- ggplot(mtcars, aes(wt, mpg))
plot + geom_line()

plot + geom_line(linetype = "dashed",color="red")
plot + geom_line(aes(color=as.factor(carb)))




#Creating Bar graphs 
library(ggplot2)
library(dplyr)
bike <- read.csv("daily-bike-rentals.csv")
bike$season <- factor(bike$season, levels = c(1,2,3,4),labels = c("Spring", "Summer", "Fall", "Winter"))
bike$workingday <- factor(bike$workingday, levels = c(0,1),labels = c("Work day", "Free day"))
bike$weathersit <- factor(bike$weathersit, levels = c(1,2,3),labels = c("Clear", "Misty/cloudy", "Light snow"))
attach(bike)

bike.sum =bike %>% 
  group_by(season, workingday) %>% 
  summarize(rental = sum(cnt))

ggplot(bike.sum, aes(x= season, y= rental)) + geom_bar(show.legend = TRUE, stat = "identity") + labs(title = "Rentals for Season and Day")

ggplot(bike.sum, aes(x= season, y= rental, fill = workingday,label = scales::comma(rental))) + 
  geom_bar(show.legend = TRUE, stat = "identity") + 
  labs(title = "Rentals for Season and Day") + 
  scale_y_continuous(labels = scales::comma) + 
  geom_text(size = 3, position = position_stack(vjust = 0.5))


ggplot(bike.sum, aes(x= season, y= rental)) + geom_bar(show.legend = TRUE, stat = "identity", fill="lightblue", colour="black"
) + labs(title = "Rentals for Season and Day")


# Making Distributions plots

library(ggplot2)
faithful=read.csv("faithful.csv")

ggplot(faithful, aes(x=waiting)) + geom_histogram()


ggplot(faithful, aes(x=waiting)) +
  geom_histogram(binwidth=5, fill="blue", colour="black")

ggplot(faithful, aes(x=waiting, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
  geom_density() +
  xlim(35, 105)

# Creating Mosaic graphs
library(stats)
mtcars=read.csv("mtcars.csv")

mosaicplot(~ gear + carb, data = mtcars, color = 2:5, las = 1)


# Making Treemaps
install.packages("treemap")
library(treemap)

branch=c(rep("branch-1",4),rep("branch-2",2),rep("brach-3",3))
subbranch=paste("subbranch" , c(1,2,3,4,1,2,1,2,3), sep="-")
value=c(13,5,22,12,11,7,3,1,23)
data=data.frame(branch,subbranch,value)


treemap(data,
        index=c("branch","subbranch"),
        vSize="value",
        type="index"
)            

post_data=read.csv("post-data.csv")

treemap(post_data,
        index=c("category", "comments"),
        vSize="views",
        type="index"
)            


# Plotting Correlations matrix
library(ggplot2)
install.packages("corrplot")
library(corrplot)

mtcars=read.csv("mtcars.csv")
rownames(mtcars) <- mtcars$X
mtcars$X=NULL

mtcars_cor <- cor(mtcars, method="pearson")
round(mtcars_cor, digits=2)

corrplot(mtcars_cor)
corrplot(mtcars_cor, method="shade", shade.col=NA, tl.col="black", tl.srt=45)


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mtcars_cor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         col=col(200), addCoef.col="black", addcolorlabel="no", order="AOE")

library(reshape2)
melted_cormat <- melt(mtcars_cor)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

# Creating Heatmaps

get_lower_triangle<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
get_upper_triangle <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- reorder_cormat(mtcars_cor)
upper_tri <- get_upper_triangle(cormat)
melted_cormat <- melt(lower_tri, na.rm = TRUE) 

ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

install.packages(c("ggmap","maps"))

library(ggmap)

tartu_housing_data <- read.csv("tartu_housing.csv", sep=";")
tartu_map <- get_map(location="tartu", maptype="satellite", zoom = 12)
ggmap(tartu_map, extent='device') +
  geom_point(aes(x=lon, y=lat), colour="yellow", alpha=0.1, size=2, data=tartu_housing_data)

# change parameters
tartu_map_g <- get_map(location="tartu", zoom = 13) 
ggmap(tartu_map_g, extent='device') +
  geom_density2d(data=tartu_housing_data, aes(x=lon, y=lat), size=.3) +
  stat_density2d(data=tartu_housing_data,
                 aes(x=lon, y=lat,  fill = ..level.., alpha = ..level..), 
                 size = 0.01, bins = 16, 
                 geom = 'polygon')+
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0, 0.30), guide = FALSE)





#See Also... Chapter-12: Where in the world? – Geospatial Analysis 


# Plotting Network graphs
install.packages("igraph")
library(igraph)


graph_directed <- graph(edges=c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6, 5,6),n=6)
graph_undirected <- graph(edges=c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6, 5,6),n=6, directed=FALSE)


par(mfrow=c(1,2))
plot(graph_directed)
plot(graph_undirected)



graph_isolation <- graph( c( "Kuntal", "Maulim", "Maulim", "Yana","Yana","Kuntal", "Ranadeep", "Rabindrika","Ranadeep", "Kuntal"), 
             
             isolates=c("Suman", "Arunava", "Raja", "Manish","Gina") )  


plot(graph_isolation, edge.arrow.size=.5, vertex.color="gold", vertex.size=15, 
     
     vertex.frame.color="gray", vertex.label.color="black", 
     
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2) 

set.seed(100)
plot(graph_directed)

# Labeling and legends
library(ggplot2)
toothgrowth=read.csv("ToothGrowth.csv")
p <- ggplot(toothgrowth, aes(x=dose, y=len)) + geom_boxplot() 

p + ggtitle("Tooth groth Length vs Dose(mg/day) ") +
  xlab("Dose of Vitamin C(mg/day)") + 
  ylab("Tooth Length")



p_leg <- ggplot(toothgrowth, aes(x=dose, y=len, fill=as.factor(dose))) + geom_boxplot() 
p_leg + labs(fill = "Dose (mg)")

p_leg + labs(fill = "Dose (mg)")+  theme(legend.position="top")

p_leg + guides(fill=FALSE)




# Coloring and themes
library(ggplot2)
toothgrowth=read.csv("ToothGrowth.csv")
p <- ggplot(toothgrowth, aes(x=dose, y=len)) + geom_boxplot() 



p + theme_bw() # balck and white

p + theme_dark()

p + theme_grey() # Grey back ground and white lines

p + theme(plot.background = element_rect(fill = "darkblue"))

p + theme(axis.text.x = element_text(face="bold", color="#993333", 
                                     size=14, angle=45),
          axis.text.y = element_text(face="bold", color="#993333", 
                                     size=14, angle=45))

p + theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())


# Multivariate chart

install.packages("GGally")
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


# Creating 3D graphs[AV3] 
install.packages("plot3D")
library(plot3D)

mtcars=read.csv("mtcars.csv")
rownames(mtcars) <- mtcars$X
mtcars$X=NULL
head(mtcars)




scatter3D(x=mtcars$wt, y=mtcars$disp, z=mtcars$mpg, clab = c("Miles/(US) gallon"))
scatter3D(x=mtcars$wt, y=mtcars$disp, z=mtcars$mpg, bty = "f",clab = c("Miles/(US) gallon"))

scatter3D(x=mtcars$wt, y=mtcars$disp, z=mtcars$mpg, pch = 18,  theta = 20, phi = 20,
          main = "Motor Trend Car Road Tests", xlab = "Weight lbs",
          ylab ="Displacement (cu.in.)", zlab = "Miles gallon")

#3D viewing direction
scatter3D(x=mtcars$wt, y=mtcars$disp, z=mtcars$mpg,clab = c("Cars Mileage"),theta = 15, phi = 0, bty ="g")




scatter3D(x=mtcars$wt, y=mtcars$disp, z=mtcars$mpg, phi = 0, bty = "g", pch = 20, cex = 0.5)
text3D(x=mtcars$wt, y=mtcars$disp, z=mtcars$mpg,  labels = rownames(mtcars),
       add = TRUE, colkey = FALSE, cex = 0.5)




#Death Rates in Virginia
data(VADeaths)

#  hist3D and ribbon3D with greyish background, rotated, rescaled,...
hist3D(z = VADeaths, scale = FALSE, expand = 0.01, bty = "g", phi = 20,
       col = "#0085C2", border = "black", shade = 0.2, ltheta = 80,
       space = 0.3, ticktype = "detailed", d = 2)

# Adding points and lines both

scatter3D(x=mtcars$wt, y=mtcars$disp, z=mtcars$mpg,type="h", clab = c("Miles/(US) gallon"))

# Selecting Graphic Device

auto <- read.csv("auto-mpg.csv")
cylinders <- factor(cylinders, levels = c(3,4,5,6,8), labels = c("3cyl", "4cyl", "5cyl", "6cyl", "8cyl"))
attach(auto)

postscript(file = "auto-boxplot.ps")
boxplot(mpg)
dev.off()

pdf(file = "auto-boxplot.pdf")
boxplot(mpg)
dev.off()

png(file="auto-boxplot.png",width=400,height=350,res=72)
boxplot(mpg)
dev.off()






