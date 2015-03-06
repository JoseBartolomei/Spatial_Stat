# TODO: Add comment
# 
# Author: jbartolomei
###############################################################################


###Produce a Probability map of the smoothed data###
####################################################
######Prepare data to be use in the DCluster package###
data.DC<-SpatialData80.84[2:6]
names(data.DC)<-c("MuniNames", "MuniCode", "Observed", "Expected", "SMR")

library(lattice)
trellis.par.set(canonical.theme(color = FALSE))



library(DCluster)
nbparam<-calculate.mle(as(data.DC, "data.frame"), model="negbin")
PR$pvalnegbin<-pnbinom(data.DC$Observed, size=nbparam$size, prob=nbparam$prob,
		lower.tail=FALSE)

###Definir parametros para construir el mapas de probabilidades###
##################################################################

colorkeypval<-list(labels=as.character(c(0, 0.01, 0.05, 0.1, .5, 1)), 
		at=(0:5)/5, height=.5)

#pvalcols<-brewer.pal(5, "Blues")
#pvalcols<-brewer.pal(5, "Greys")
# RSB quieting greys
pvalcols <- grey.colors(5, 0.95, 0.55, 2.2)


print(spplot(PR, c("pvalpois","pvalnegbin"), col.regions=rev(pvalcols), 
				at=c(0, 0.01, 0.05, 0.1, .5, 1), axes=TRUE, colorkey=colorkeypval ))


library(DCluster)
library(spdep)

Spatial.D<-data.frame(c(obsMuni.P[1], ExpMuni.P[1]))
PR.D$Observed<-Spatial.D$Observed
PR.D$Expected<-Spatial.D$Expected
pmap<-probmap(PR.D$Observed, PR.D$Expected)
spplot(pmap$pmap)