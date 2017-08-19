Plot.rsx
———————
##Point pattern analysis=group
##showplots
##Layer=vector
##X=Field Layer
##Y=Field Layer
##Group=Field Layer
require(ggplot2)
ggplot()+
geom_point(aes(x=Layer[[X]],y=Layer[[Y]],
color=as.factor(Layer[[Group]])))+
theme(legend.title = element_blank())+
xlab(X)+
ylab(Y)

RandomSampling.rsx
——————————————
##Point pattern analysis=group
##Layer=vector
##Size=number 10
##Output= output vector
pts=spsample(Layer,Size,type="random")
Output=SpatialPointsDataFrame(pts, as.data.frame(pts))

Ripley-Rasson.rsx
——————————————
##[Point pattern analysis]=group
##Layer=vector
##Output=output vector
library("spatstat")
library("maptools")
proj4string(Layer)->crs
spatpoints = as(Layer,"SpatialPoints")
ripras=ripras(as(spatpoints,"ppp"))
polyg=as(ripras,"SpatialPolygons")
Output1= SpatialPolygonsDataFrame(polyg, data.frame(1))
proj4string(Output1)<-crs
Output<-Output1
