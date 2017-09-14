# Code snippets for Chapter 5 of R Data analysis cookbook-2nd Edition
# ===================================================================


#Recipe: Performing Cluster analysis using hierarchical clustering
#--------------------------------------------------------------

proteinIntake <- read.csv("protein.csv")
head(proteinIntake)

proteinIntakeScaled = as.data.frame(scale(proteinIntake[,-1]))
proteinIntakeScaled$Country =proteinIntake$Country

hc = hclust(dist(proteinIntakeScaled, method="euclidean"), method="ward.D2")
hc
plot(hc, hang = -0.01, cex = 0.7)

hc2 = hclust(dist(proteinIntakeScaled), method="single")
plot(hc2, hang = -0.01, cex = 0.7)

install.packages("cluster")
library(cluster)
dv = diana(proteinIntakeScaled, metric = "euclidean")
plot(dv)

fit = cutree(hc, k = 4)
table(fit)
plot(hc)
rect.hclust(hc, k = 4 , border="red")


#Recipe: Performing Cluster analysis using k-means clustering
#----------------------------------------------------------
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")

proteinIntake <- read.csv("protein.csv")
rownames(proteinIntake)=proteinIntake$Country
proteinIntake$Country=NULL
proteinIntakeScaled = as.data.frame(scale(proteinIntake))


set.seed(22)
kmFit = kmeans(proteinIntakeScaled, 4)
kmFit

aggregate(proteinIntakeScaled, by=list(cluster=kmFit$cluster), mean)
library(factoextra)
fviz_cluster(kmFit, data = proteinIntakeScaled)

fviz_nbclust(proteinIntakeScaled, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

install.packages(c("OpenImageR","ClusterR"))
library(OpenImageR)
library(ClusterR)

img = readImage("bird.jpg")
img_resize = resizeImage(img, 350, 350, method = 'bilinear') 
imageShow(img_resize) 

img_vector = apply(img_resize, 3, as.vector)                                # vectorize RGB
dim(img_vector)

km_mb = MiniBatchKmeans(img_vector, clusters = 5, batch_size = 20, num_init = 5, max_iters = 100, 
                        init_fraction = 0.2, initializer = 'kmeans++', early_stop_iter = 10,
                        verbose = F)
pr_mb = predict_MBatchKMeans(img_vector, km_mb$centroids)

getcent_mb = km_mb$centroids

new_im_mb = getcent_mb[pr_mb, ]
dim(new_im_mb) = c(nrow(img_resize), ncol(img_resize), 3)

imageShow(new_im_mb)

library("cluster")

pamFit <- pam(proteinIntakeScaled, 4)
pamFit$medoids

fviz_cluster(pamFit)

# Compute clara
claraFit <- clara(proteinIntakeScaled, 4, samples=5)
claraFit$medoids
fviz_cluster(claraFit)


#Recipe: Performing cluster validation
#-----------------------------------

install.packages(c("factoextra","fpc","cluster","NbClust"))
library(factoextra)
library(cluster)
library(fpc)
library(NbClust)

proteinIntake <- read.csv("protein.csv")
rownames(proteinIntake)=proteinIntake$Country
proteinIntake$Country=NULL
proteinIntakeScaled = as.data.frame(scale(proteinIntake))

nb <- NbClust(proteinIntakeScaled, distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "ward.D2", index ="all")
fviz_nbclust(nb) + theme_minimal()

km.res = kmeans(proteinIntakeScaled, 3)
sil.km <- silhouette(km.res$cluster, dist(proteinIntakeScaled))
# Summary of silhouette analysis
si.sum <- summary(sil.km )
# Average silhouette width of each cluster
si.sum$clus.avg.widths
# The total average (mean of all individual silhouette widths)
si.sum$avg.width
# The size of each clusters
si.sum$clus.sizes

fviz_silhouette(sil.km)

pam.res <- pam(proteinIntakeScaled, 3)

dd <- dist(proteinIntakeScaled, method ="euclidean")
# Statistics for pam clustering
pam_stats <- cluster.stats(dd,  pam.res$cluster)
# (pam) within clusters sum of squares
pam_stats$within.cluster.ss

# (pam) cluster average silhouette widths and dunns index
pam_stats$clus.avg.silwidths
pam_stats$dunn
pam_stats$dunn2

res.stat <- cluster.stats(dd, km.res$cluster, pam.res$cluster)

res.stat$corrected.rand
res.stat$vi

#Recipe: Advance clustering
#------------------------

install.packages("fpc")
library(fpc)

data("multishapes", package = "factoextra")
dataPoints <- multishapes[, 1:2]
head(dataPoints)
plot(dataPoints)
dsFit <- dbscan(dataPoints, eps = 0.15, MinPts = 5)
print(dsFit)
fviz_cluster(dsFit, dataPoints, geom = "point")

install.packages("mclust")
library(mclust)

mm = Mclust(dataPoints)
plot(mm)
summary(mm)


#Recipe: Reducing dimensionality with Principal Component Analysis (PCA)
#---------------------------------------------------------------------
bh <- read.csv("BostonHousing.csv") 

install.packages("corrplot")
library(corrplot)

corr <- cor(bh[,-14])
corrplot(corr, method="color")

bh.pca <- prcomp(bh[,-14], scale = TRUE) 

print(bh.pca)

summary(bh.pca) 

plot(bh.pca)
plot(bh.pca, type = "lines") 
biplot(bh.pca, col = c("gray", "black")) 

bh.pca$rotation 
bh.pca$sdev 

