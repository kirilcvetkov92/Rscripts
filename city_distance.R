rm(list = ls())
library(remote)
city_data <- read.csv("simplemaps-worldcities-basic.csv")

getDist <- function(lat1,lng1,lat2,lng2){
  r=6371
  lat1 <- deg2rad(lat1)
  lat2 <- deg2rad(lat2)
  lat_dif <- lat2-lat1
  lng_dif <- deg2rad(lng2-lng1)
  a <-sin(lat_dif/2.0)**2+cos(lat1)*cos(lat2)*sin(lng_dif/2.0)**2
  d <- 2*r*asin(sqrt(a))
  return(d)
}
mat <- matrix(0.0, nrow = nrow(city_data), ncol = nrow(city_data))
for (i in 1:nrow(city_data)){
  for (j in 1:nrow(city_data))
    mat[i,j] <- getDist(city_data[i,3],city_data[i,4],city_data[j,3],city_data[j,4])
}
city_dist <- as.dist(mat)
city_projection <- cmdscale(city_dist,eig=TRUE,k=1)
lin_cities <- city_projection$points[,1]
names(lin_cities) <- city_data$city
save(city_dist,file="city_dist.Rdata")
#load("city_dist.Rdata")
#-------------------------------------------
library(maps)
library(geosphere)
library(tidyverse)
library(MASS)
world..cities <- world.cities %>%
  filter(world.cities$capital == 1)
Caps = cbind(world..cities$long, world..cities$lat)
CapDistMatrix = distGeo(Caps,Caps)
city_dist <- as.dist(CapDistMatrix)
save(city_dist,file="city_dist.Rdata")
city_projection <- isoMDS(city_dist,k=1)
city_projection <- isoMDS(CapDistMatrix,k=1,maxit = 500)
city_projection <- cmdscale(CapDistMatrix, k = 1, eig = FALSE, add = TRUE, x.ret = FALSE)
lin_cities <- city_projection$points[,1]
names(lin_cities) <- world..cities$name
library(tsne)
city_projection <- tsne(city_dist,k = 1)
pco(CapDistMatrix)


pdi <- read.csv("power-dist-index.csv",sep=';',na.strings = "#NULL!")
#pdi_data <- data.frame(lapply(pdi, function(x) 
#  {gsub("#NULL!",NA,x)}))
pdi_data <- na.omit(pdi)
pdi_data[,c("pdi","idv","mas","uai","ltowvs","ivr")] <- as.numeric(as.character(unlist(pdi_data[,c("pdi","idv","mas","uai","ltowvs","ivr")])))


pdi.pca <- prcomp(pdi_data[,3:8],center = TRUE, scale = FALSE)
pdi_cities <- pdi.pca$x[,1]
names(pdi_cities) <- pdi_data$country

isoMDS(dist(as.matrix((pdi_data[,3:8]))))

bla <- tsne(pdi_data[,3:8],whiten = TRUE, k = 1,perplexity = 10, max_iter = 3000, initial_dims = 50)
bla <- prcomp(pdi_data[,3:8], center = TRUE, scale. = TRUE)
bla <- prcomp(pdi_data[,3:8], center = FALSE, scale. = FALSE)
pca.bla <- bla$x[,1]
rownames(pca.bla) <- pdi_data$country
library(caret)
trans <- preProcess(pdi_data[,3:8],method=c("BoxCox", "center", 
                                            "scale", "pca"))
PC = predict(trans, pdi_data[,3:8])

#-----continent diff----looks fairly okay :D----------
continent_diff <- read.csv("continent_diff.csv",sep=';')
rownames(continent_diff) <- continent_diff$X
continent_diff$X <- NULL
continent_diff <- as.dist(continent_diff)

#continent_projection <- cmdscale(continent_diff,eig=FALSE,k=1)

con_projection <- isoMDS(continent_diff,k=1)
continent_projection <- con_projection$points[,1]

#------------------------------------

library(geosphere)
library(maps)
library(reshape)  
library(data.table)
library(MASS)
# Load world cities data and keep only 300 cities, which willgive us 90,000 pairs
data(world.cities)
world.cities <- subset(world.cities, capital == 1)

# create all possible pairs of origin-destination in a long format
dt <- expand.grid.df(world.cities,world.cities)
names(dt)[10:11] <- c("lat_dest","long_dest")
names(dt)[7] <- "name2"
# calculate distances in meters:
setDT(dt)[ , dist_km := distGeo(matrix(c(long, lat), ncol = 2), 
                                matrix(c(long_dest, lat_dest), ncol = 2))/1000]

dfr <- reshape(dt[,c("name","name2","dist_km")], direction="wide", idvar="name",timevar="name2")

d <- as.dist(dfr[, -1])

#attr(d, "Labels") <- dt$name
bla <- isoMDS(d,k=1)
blu <- (bla$points[,1])
bla <- cmdscale(d,k=1)
blu <- bla[,1]

library(vegan)

bla <- metaMDS(d)
names(blu) <- gsub("dist_km.","",names(blu))
blu[names(blu) == "Bern"]
blu[blu < -1400 & blu > -1600]


