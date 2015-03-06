# TODO: Add comment
# 
# Author: jbartolomei
###############################################################################




library(sp)
library(spdep)
library(maptools)

##  Read Puerto rico Shapefile and convert to R polygon
PR_map<-readShapePoly(fn="C:\\Documents and Settings\\Owner\\My Documents\\Downloads\\R\\Puerto_Rico\\data\\co72_d90")

#Ensure that we have only 78 regions, some of them having
#several polygons. We need this to compute the adjacency
#structure properly
PRmap<-unionSpatialPolygons(PR_map, IDs=as.character(PR_map@data$CO))

d<-data.frame(CO=unique(as.character(PR_map$CO)), 
		NAME=unique(as.character(PR_map$NAME)))

rownames(d)<-d$CO

PRmap<-SpatialPolygonsDataFrame(PRmap, d)

#save Puerto Rico Spatial Structure

save(file="C:\\Program Files\\eclipse\\workspace\\Spatial_Stat\\Images\\PR_Spatial_Strucure.RData", list=c("PRmap"))




