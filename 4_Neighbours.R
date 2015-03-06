# TODO: Add comment
# 
# Author: jbartolomei
###############################################################################
load(file="C:\\Program Files\\eclipse\\workspace\\Spatial_Stat\\Images\\PR_Spatial_Strucure.RData")

library(sp)
library(spdep)
library(maptools)

## Construct neighbours list from polygon list. 
##  neighbours list based on regions with contiguous boundaries, that is sharing one or more boundary point.

PR_nb1<-poly2nb(as(PRmap, "SpatialPolygons"))

##  Creat an object with coordinates only. This is needed to map neighbours
coords<-coordinates(PRmap)

##  Plot neighbours
plot(PR_nb1, coords, col="red")
title(main="Contiguity Neighbours")
##  Graph-Based Neighbours

IDs<-row.names(as(PRmap, "data.frame"))
library(tripack)


##  Graph object containing a list with the vertex coordinates and the to and from indices defining the edges. 
##  The helper function graph2nb converts a graph object into a neighbour list. 
PR_nb2<-tri2nb(coords, row.names = IDs)## convert a matrix of two-dimensional coordinates into a neighbours 
##  list of class nb with a list of integer vectors containing neighbour region number ids. 
PR_nb3<-graph2nb(soi.graph(PR_nb2, coords), row.names = IDs)
PR_nb4<-graph2nb(gabrielneigh(coords), row.names = IDs)
PR_nb5<-graph2nb(relativeneigh(coords), row.names = IDs)

par(mfrow=c(4,1))

plot(PR_nb2, coords, col="red", title(xlab="Delauney Triangulation"))
title(main="Graph Based Neigbours")
plot(PR_nb3, coords, col="red", title(xlab="Sphere of Influence"))
plot(PR_nb4, coords, col="red", title(xlab="Gabriel Graph"))
plot(PR_nb5, coords, col="red", title(xlab="Relative Graph"))


##  Create Distance-Based Neighbours

PR_nb6D<-knn2nb(knearneigh(coords, k=1), row.names = IDs)
PR_nb7D<-knn2nb(knearneigh(coords, k=2), row.names = IDs)
PR_nb8D<-knn2nb(knearneigh(coords, k=4), row.names = IDs)
PR_nb9D<-knn2nb(knearneigh(coords, k=8), row.names = IDs)

##  Plot Distance Based Neighbours
par(mfrow=c(4,1))

plot(PR_nb6D, coords, col="red")
title(main="Distance Based Neighbours", xlab="1,000 meters")
plot(PR_nb7D, coords, col="red")
title(xlab="2,000 meters")
plot(PR_nb8D, coords, col="red")
title(xlab="4,000 meters")
plot(PR_nb9D, coords, col="red")
title(xlab="8,000 meters")

col.knn <- knearneigh(coords, k=1)

centroids <- getSpPPolygonsLabptSlots(PR.D)
PR.D$x<-centroids[,1]
PR.D$y<-centroids[,2]
#save Puerto Rico Spatial Neighbours

save(file="C:\\Program Files\\eclipse\\workspace\\Spatial_Stat\\Images\\PR_Spatial_Neighbours.RData", 
		list=c("PR_nb1", "PR_nb2", "PR_nb3", "PR_nb4", "PR_nb5", "PR_nb6D","PR_nb7D", "PR_nb8D", "PR_nb9D"))
