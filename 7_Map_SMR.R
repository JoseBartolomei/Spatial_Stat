# TODO: Add comment
# 
# Author: jbartolomei
###############################################################################

## Load calculated spatial data
load("C:\\Program Files\\eclipse\\workspace\\Spatial_Stat\\Asthma_Mortality\\Data\\Spatial_Data.RData")

## Load Puerto Rico Spatial Structure

load("C:\\Program Files\\eclipse\\workspace\\Spatial_Stat\\Images\\PR_Spatial_Strucure.RData")

library(sp)

########### Map for Asthma Mortality by Municipality of Residence ######################


PR.D<-SpatialPolygonsDataFrame(PRmap, data.frame(SMR.P1=SMR.Muni.P[[1]], SMR.P2=SMR.Muni.P[[2]], 
		SMR.P3=SMR.Muni.P[[3]], SMR.P4=SMR.Muni.P[[4]]),match.ID=FALSE)


###################################################
### chunk number 2: 
###################################################

library(RColorBrewer)

brks<-as.list(rep(NA,4))
atcol<-as.list(rep(NA,4))
colorkey<-as.list(rep(NA,4))

for (i in 1:4)
{
	brks[[i]]<-exp(quantile(log(c(SMR.Muni.P[[i]])), 0:5/5 ))
	brks[[i]]<-brks[[i]][2:6]

	atcol[[i]]<-(0:4)*brks[[i]][5]/5
	
	colorkey[[i]]<-list(labels=as.character(c(formatC(brks[[i]], format="f", dig=2))),
			at=atcol[[i]],  height=.5)

}

cols <- brewer.pal(5, "Oranges")

##  Display map with individuals scale based on their data distribution
print(spplot(PR.D, c("SMR.P1"), main="SMR 1989-1993", names.attr=c("SMR"),
				col.regions=cols, at=brks[[1]], axes = TRUE, colorkey=colorkey[[1]]))


print(spplot(PR.D, c("SMR.P2"), main="SMR 1994-1998", names.attr=c("SMR"),
				col.regions=cols, at=brks[[2]], axes = TRUE, colorkey=colorkey[[2]]))


print(spplot(PR.D, c("SMR.P3"), main="SMR 1999-2003", names.attr=c("SMR"),
				col.regions=cols, at=brks[[3]], axes = TRUE, colorkey=colorkey[[3]]))

print(spplot(PR.D, c("SMR.P4"), main="SMR 2004-2008", names.attr=c("SMR"),
				col.regions=cols, at=brks[[4]], axes = TRUE, colorkey=colorkey[[4]]))
		

## Display maps with a single scale based on the 1989-1993 death distribution


print(spplot(PR.D, c("SMR.P4", "SMR.P3", "SMR.P2", "SMR.P1"), 
				main="Age-Sex adjusted Standardized Mortality Ratio, 1989-2008", 
				names.attr=c("2004-2008", "1999-2003", "1994-1998", "1989-1993"),
				col.regions=cols, at=brks[[1]], axes = TRUE, colorkey=colorkey[[1]]))



##  Calcular p-values por municipio y periodo
d.pois<-(as.list(rep(NA, 4)))

for(i in 1:4)
{
	d.pois[[i]]<-ppois(d[[i]]$Observed, d[[i]]$Expected, lower.tail=FALSE)
}

PR.D$pois.P1<-d.pois[[1]]
PR.D$pois.P2<-d.pois[[2]]
PR.D$pois.P3<-d.pois[[3]]
PR.D$pois.P4<-d.pois[[4]]

	brks<-c(0, 0.01, 0.05, 0.10, 0.5, 1 )
	
	
	atcol<-(0:5)*brks[6]/5
	
	colorkey<-list(labels=as.character(c(formatC(brks, format="f", dig=2))),
			at=atcol,  height=.5)
	

cols <- brewer.pal(6, "Oranges")

print(spplot(PR.D, c("pois.P4", "pois.P3", "pois.P2", "pois.P1"), 
				main="P values of the Age-Sex adjusted Standardized Mortality Ratio, 1989-2008", 
				names.attr=c("2004-2008", "1999-2003", "1994-1998", "1989-1993"),
				col.regions=cols, at=brks, axes = TRUE, colorkey=(colorkey)))



###################################################
### chunk number 3: 
###################################################

centroids <- getSpPPolygonsLabptSlots(PR.D)
PR.D$x<-centroids[,1]
PR.D$y<-centroids[,2]


save.image(file="C:\\Program Files\\eclipse\\workspace\\Spatial_Stat\\Spatial_Data.RData")

