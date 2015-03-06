# TODO: Add comment
# 
# Author: jbartolomei
###############################################################################

## rm(list=ls(all=TRUE))

 ## Load image with prepared data set for analysis
 
load("C:\\Documents and Settings\\jbartolomei\\My Documents\\PRASS\\Mortality\\Analysis\\Spatial_Analysis\\R_image\\Mort_89_08.RData")
 
Mort <- read.csv("/media/truecrypt2/ORP2/Vital_Statistics/Mortality/Spatial_Stat/Data/Deaths/MortResi89.08_5_54years.csv")
## Observed cases by municipality, age sex-group and year

obs <- by(Mort[,1], list(AGESEXG = Mort$AgeSexG, CODMUNI = Mort$"CODMUNI", 
				YEAR = Mort$"YEAR"), length)

obs[is.na(obs)] <- 0

obs[1, 25, 1] <- 0 # Remove fake case
Mort <- Mort[-nrow(Mort),]

obs[1, 35, 1] <- 0 # Remove fake case
Mort <- Mort[-nrow(Mort),]

obs[1, 56, 1] <- 0 # Remove fake case
Mort <- Mort[-nrow(Mort),]


##  Observed death by age-sex group and municipality, 1989-2008 (aggregated)
obsAgeSexMuni.89.08 <- apply(obs[,,], c(2,1), sum)

##  Observed death by age-sex group, municipality and time period.

obsAgeSexMuni.P <- as.list(rep(NA, 4))

obsAgeSexMuni.P[[1]] <- apply(obs[,,1:5], c(2,1), sum)
obsAgeSexMuni.P[[2]] <- apply(obs[,,6:10], c(2,1), sum)
obsAgeSexMuni.P[[3]] <- apply(obs[,,11:15], c(2,1), sum)
obsAgeSexMuni.P[[4]] <- apply(obs[,,16:20], c(2,1), sum)

##  Observed deaths by municipality and time perid

obsMuni.P <- as.list(rep(NA, 4))

obsMuni.P[[1]] <- apply(obs[,,1:5], 2, sum)
obsMuni.P[[2]] <- apply(obs[,,6:10], 2, sum)
obsMuni.P[[3]] <- apply(obs[,,11:15], 2, sum)
obsMuni.P[[4]] <- apply(obs[,,16:20], 2, sum)

##  Load Population

popmales <- as.list(rep(NA, 4))
popfemales <- as.list(rep(NA, 4))

#  Period 1 = year 1989 to 1993
popmales[[1]] <- read.csv(file="C:\\Pop_males_89_93.csv", header=TRUE, sep=",") 
popfemales[[1]] <- read.csv(file="C:\\Pop_females_89_93.csv", header=TRUE, sep=",")

#  Period 2 = year 1994 to 1998 ** End of ICD-9 **
popmales[[2]] <- read.csv(file="C:\\Pop_males_94_98.csv", header=TRUE, sep=",") 
popfemales[[2]] <- read.csv(file="C:\\Pop_females_94_98.csv", header=TRUE, sep=",")

#  Period 3 = year 1999 to 2003 ** Year 1999 is the begining of the ICD-10 **
popmales[[3]] <- read.csv(file="C:\\Pop_males_99_03.csv", header=TRUE, sep=",") 
popfemales[[3]] <- read.csv(file="C:\\Pop_females_99_03.csv", header=TRUE, sep=",")

#  Period 4 = year 2004 to 2008
popmales[[4]] <- read.csv(file="C:\\Pop_males_04_08.csv", header=TRUE, sep=",")
popfemales[[4]] <- read.csv(file="C:\\Pop_females_04_08.csv", header=TRUE, sep=",")


##  Population by municipality bye age-sex group from the year 1989 to 2008.

PopAgeSex.89.08 <- 
  cbind(popmales[[1]][3:7] + popmales[[2]][3:7] + 
          popmales[[3]][3:7] + popmales[[4]][3:7],
        popfemales[[1]][3:7]+ popfemales[[2]][3:7] +
          popfemales[[3]][3:7]+ popfemales[[4]][3:7])


## Standard Rate using death and pop from 1989 to 2008
Standard.Rate.89.08 <- (obsAgeSexMuni.89.08/PopAgeSex.89.08) * 100000

## Population by municipality and time periods.

PopMuni <- as.list(rep(NA, 4))

for(i in 1:4)
{
PopMuni[[i]]<- apply(cbind(popmales[[i]][3:7], popfemales[[i]][3:7]), 1, sum)

}

## Population by age-sex group, municipality and time periods.
PopAgeSex.P <- as.list(rep(NA, 4))

PopAgeSex.P[[1]] <- cbind(popmales[[1]][3:7], popfemales[[1]][3:7])
PopAgeSex.P[[2]] <- cbind(popmales[[2]][3:7], popfemales[[2]][3:7])
PopAgeSex.P[[3]] <- cbind(popmales[[3]][3:7], popfemales[[3]][3:7])
PopAgeSex.P[[4]] <- cbind(popmales[[4]][3:7], popfemales[[4]][3:7])

## Expected Cases by age-sex group, municipality and time period

ExpAgeSex.P <- as.list(rep(NA, 4))

for (i in 1:4)
{
	ExpAgeSex.P[[i]] <- (Standard.Rate.89.08 * PopAgeSex.P[[i]]) / 100000
}

##  Expected rate by municipality and time periods

ExpMuni.P <- as.list(rep(NA, 4))

for (i in 1:4)
{
	ExpMuni.P[[i]] <- apply(ExpAgeSex.P[[i]], 1, sum) + 0.001
}

## I added 0.001 to fake value in municipalities 25, 35 and 56 to avoid NA in SMR


SMR.Muni.P<- as.list(rep(NA, 4))

for (i in 1:4)
{
	SMR.Muni.P[[i]] <- (obsMuni.P[[i]]/ExpMuni.P[[i]])
}



SpatialData <- 
  cbind(codmuni, Observed=obsMuni.P[[1]], 
        Expected=ExpMuni.P[[1]], 
        SMR=SMR.Muni.P[[1]])

save(file="C:\\Program Files\\eclipse\workspace\\Spatial_Stat\\Images\\Spatial_Data.RData",
     list=c("codmuni", "obsMuni.P" , "ExpMuni.P", "SMR.Muni.P"))

