# TODO: Add comment
# 
# Author: jbartolomei
###############################################################################

load((file="C:\\Program Files\\eclipse\\workspace\\Spatial_Stat\\Images\\Spatial_Data.RData"))
load(file="C:\\Program Files\\eclipse\\workspace\\Spatial_Stat\\Images\\PR_Spatial_Neighbours.RData")
###################################################
### chunk number :  Selection of observed and expected for every period
###################################################


################## Global Tests for spatial autocorrelation (Moran's I, Geary's C and Getis-Ord) #########

## Moran's I test for spatial autocorrelation
	moran.test(SMR.Muni.P[[1]], listw=PR_W1W, randomisation=TRUE,, zero.policy=TRUE)
	moran.test(SMR.Muni.P[[2]], listw=PR_W1W, randomisation=TRUE,, zero.policy=TRUE)
	moran.test(SMR.Muni.P[[3]], listw=PR_W1W, randomisation=TRUE,, zero.policy=TRUE)
	moran.test(SMR.Muni.P[[4]], listw=PR_W1W, randomisation=TRUE,, zero.policy=TRUE)

	##  Different spatial weights in period 1
	moran.test(SMR.Muni.P[[1]], listw=PR_W1B, randomisation=TRUE,, zero.policy=TRUE)
	moran.test(SMR.Muni.P[[1]], listw=PR_W1S, randomisation=TRUE,, zero.policy=TRUE)
	moran.test(SMR.Muni.P[[1]], listw=PR_W8W, randomisation=TRUE,, zero.policy=TRUE)

## note: No autocorrelation was found using different spatial weights

##  Geary's c test for sptaial autocorrelation

	geary.test(SMR.Muni.P[[1]], listw=PR_W1W, randomisation=TRUE, zero.policy=TRUE)
	geary.test(SMR.Muni.P[[2]], listw=PR_W1W, randomisation=TRUE, zero.policy=TRUE)
	geary.test(SMR.Muni.P[[3]], listw=PR_W1W, randomisation=TRUE, zero.policy=TRUE)
	geary.test(SMR.Muni.P[[4]], listw=PR_W1W, randomisation=TRUE, zero.policy=TRUE)

## Global Getis-Ord G for spatial a

	globalG.test(SMR.Muni.P[[1]], listw=PR_W1B, zero.policy=TRUE)
	globalG.test(SMR.Muni.P[[2]], listw=PR_W1B, zero.policy=TRUE)
	globalG.test(SMR.Muni.P[[3]], listw=PR_W1B, zero.policy=TRUE)
	globalG.test(SMR.Muni.P[[4]], listw=PR_W1B, zero.policy=TRUE)

library(DCluster)

niter<-100

d<-as.list(rep(NA,4))

for (i in 1:4)
{
	d[[i]]<-data.frame(Observed=obsMuni.P[[i]], Expected=ExpMuni.P[[i]])
}


###################################################
### chunk number 8:  Bootstrap resampling
###################################################

moran.boot<-as.list(rep(NA,4))

for (i in 1:4)
{
	moran.boot[[i]]<-boot(d[[i]], statistic=moranI.boot, R=niter, listw=PR_W1W, n=length(PR_nb1), 
			S0=Szero(PR_W1W), zero.policy=TRUE )
}

summary(moran.boot[[1]])

par(mfrow=c(4,1))
plot(moran.boot[[1]])#Display results
plot(moran.boot[[2]])
plot(moran.boot[[3]])
plot(moran.boot[[4]])

##############  Local Test for Spatial Autocorrelation  ################
par(mfrow=c(2,2))

moran.plot(obsMuni.P[[1]], listw = PR_W1W)
title(main="Moran plot for different spatial Weights (W,B,C,S),
for the asthma mortality death of period 1.")
moran.plot(obsMuni.P[[1]], listw = PR_W1B)
moran.plot(obsMuni.P[[1]], listw = PR_W1C)
moran.plot(obsMuni.P[[1]], listw = PR_W1S)


