# 1 Downloading and plotting  a Google map of an area
install.packages("RgoogleMaps")
library(RgoogleMaps)

shu.map <- GetMap(center = c(40.742634, -74.246215), zoom=17)
PlotOnStaticMap(shu.map)

shu.map = GetMap(center = c(40.742634, -74.246215), zoom=16, destfile = "shu.jpeg", format = "jpeg")

shu.map = GetMap(center = c(40.742634, -74.246215), zoom=16, destfile = "shu.jpeg", format = "jpeg", maptype = "satellite")
PlotOnStaticMap(shu.map)

# 2. Overlaying data on downloaded Google map

library(RgoogleMaps)
wages <- read.csv("nj-wages.csv")
wages$wgclass <- cut(wages$Avgwg, quantile(wages$Avgwg, probs=seq(0,1,0.2)), labels=FALSE, include.lowest=TRUE)
pal <- palette(rainbow(5))
attach(wages)
MyMap <- MapBackground(lat=Lat, lon=Long)
PlotOnStaticMap(MyMap, Lat, Long, pch=21, cex = sqrt(wgclass),bg=pal[wgclass])
legend("bottomright", legend=paste("<=", round(tapply(Avgwg, wgclass, max))), pch=21, pt.bg=pal, pt.cex=1.0, bg="gray", title="Avg wgs")


install.packages(c("ggmap","maps"))
# If you get ggmap Error: GeomRasterAnn was built with an incompatible version of ggproto,
#Then reinstall your ggmap packgae from source
#install.packages("ggmap", type = "source")
library(maps)
library(ggmap)

crimes <- read.csv("chicago-crime.csv")

chicago = get_map(location = "chicago", zoom = 11)
ggmap(chicago)
ggmap(chicago) + geom_point(data = crimes[1:100,], aes(x = Longitude, y = Latitude))

crimes$Date = strptime(crimes$Date, format = "%m/%d/%y %H:%M")

LatLonCounts = as.data.frame(table(round(crimes$Longitude, 2), round(crimes$Latitude,2)))


LatLonCounts$Lon = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))

ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Lon, y = Lat, color = Freq, size = Freq)) + scale_color_gradient(low = "yellow", high = "red")

ggmap(chicago) + geom_tile(data = LatLonCounts, aes(x = Lon, y = Lat, alpha = Freq), fill = "red")


#3. Importing ESRI shape files into R

install.packages("sp")
install.packages("rgdal")
library(sp)
library(rgdal)
countries_sp <- readOGR(".", "ne_50m_admin_0_countries")
class(countries_sp)

airports_sp <- readOGR(".", "ne_50m_airports")
class(airports_sp)

#4. Using the sp package to plot geographic data

library(sp)
library(rgdal)
countries_sp <- readOGR(".", "ne_50m_admin_0_countries")

airports_sp <- readOGR(".", "ne_50m_airports")

plot(countries_sp)
plot(countries_sp, col = countries_sp@data$admin)

plot(airports_sp, add=TRUE)
spplot(countries_sp, c("economy"))

spplot(countries_sp, c("pop_est"))

#5. Getting maps from the maps package

install.packages("maps")
library(maps)

map("world")
map("world", interior=FALSE)

map("world", fill=TRUE, col=palette(rainbow(7)))
map("world", "tanzania")

map("france")
map("italy")

map("state")
map("state", interior = FALSE)
map("county")

map("state", "new jersey")
map("county", "new jersey")

#6. Creating spatial data frames from regular data frames containing spatial and other data

nj <- read.csv("nj-wages.csv")
class(nj)
coordinates(nj) <- c("Long", "Lat")
class(nj)
plot(nj)

nj.lines <- SpatialLines(list(Lines(list(Line(coordinates(nj))), "linenj")))
plot(nj.lines)

#7. Creating spatial data frames by combining regular data frames with spatial objects
install.packages("maptools")
library(map)
library(maptools)

nj.map <- map("county", "new jersey", fill=TRUE, plot=FALSE)
str(nj.map)
county_names <- sapply(strsplit(nj.map$names, ","), function(x) x[2])
nj.sp <- map2SpatialPolygons(nj.map, IDs = county_names, proj4string = CRS("+proj=longlat +ellps=WGS84"))
class(nj.sp)

nj.dat <- read.csv("nj-county-data.csv")
rownames(nj.dat) <- nj.dat$name
nj.spdf <- SpatialPolygonsDataFrame(nj.sp, nj.dat)
class(nj.spdf)

plot(nj.spdf)
spplot(nj.spdf, "population", main = "Population")
spplot(nj.spdf, c("per_capita_income","median_family_income"), main = "Incomes")

#8. Adding variables to an existing spatial data frame

library(map)
library(maptools)

nj.map <- map("county", "new jersey", fill=TRUE, plot=FALSE)
county_names <- sapply(strsplit(nj.map$names, ","), function(x) x[2])
nj.sp <- map2SpatialPolygons(nj.map, IDs = county_names, proj4string = CRS("+proj=longlat +ellps=WGS84"))
nj.dat <- read.csv("nj-county-data.csv")
rownames(nj.dat) <- nj.dat$name
nj.spdf <- SpatialPolygonsDataFrame(nj.sp, nj.dat)

pop_density <- nj.spdf@data$population/nj.spdf@data$area_sq_mi
nj.spdf <- spCbind(nj.spdf, pop_density)
names(nj.spdf@data)
spplot(nj.spdf, "pop_density")




