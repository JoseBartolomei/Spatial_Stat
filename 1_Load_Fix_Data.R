# TODO: Add comment
# 
# Author: jbartolomei
###############################################################################

## This is Script #1 to perform Spatial analysis on asthma mortality data by municipality of residence
## from the year 1989 to 2008 in the 5 to 54 age group.

## The analysis will be performed by 5 year period and the age group will be stratified by five year age group. 

## 1_Load asthma mortality data by municipality of residence from 1989 to 2008 for 5 to 54 year of age.
## In addition inconsistencies, if found, will be fixed.

Mort<-read.csv("C:\\Documents and Settings\\jbartolomei\\My Documents\\PRASS\\Mortality\\Databases\\MortResi89_08_5_54years.csv")

## Veryfy class
class(Mort)

## summary data to observe variable distribution and data error to be fixed

summary(Mort)

## (JAB)Upper case muni.resi names were found
table(Mort$muni.resi)

Mort$muni.resi<-tolower(Mort$muni.resi)

## (Note) Some municipality were wrote in two different ways

## Fix municipality names

Mort$muni.resi[Mort$muni.resi=="bayamón"]<-"bayamon"
Mort$muni.resi[Mort$muni.resi=="canóvanas"]<-"canovanas"
Mort$muni.resi[Mort$muni.resi=="loíza"]<-"loiza"
Mort$muni.resi[Mort$muni.resi=="manatí"]<-"manati"
Mort$muni.resi[Mort$muni.resi=="río grande"]<-"rio grande"
Mort$muni.resi[Mort$muni.resi=="san germán"]<-"san german"

## verify changes with a table
table(Mort$muni.resi)

##  (Note) There are thre (3) municipalities with cero (0) death.  
##  (Note) Furhtermore do not appear in the municipality list.

#  Add fake cases so that the missing municipalities are included in the data set

Mort<-rbind(Mort, Mort[1,])
Mort[nrow(Mort), "muni.resi"]<-c("culebra") 

Mort<-rbind(Mort, Mort[1,])
Mort[nrow(Mort), "muni.resi"]<-c("hormigueros")

Mort<-rbind(Mort, Mort[1,])
Mort[nrow(Mort), "muni.resi"]<-c("patillas") 

## (Note) Remember that three (3) rows were added. Need to take this into account in the analysis


#Create a collum with the Code for the MUNICIPALITies

muninames<-sort(as.character(unique(Mort$"muni.resi")))
muninames

#Code of the MUNICIPALITY
codmuni<-data.frame(NAME=muninames,
		MUNICIP=1:78)

Mort$CODMUNI<-match(Mort$"muni.resi",codmuni[,1])

## Create a collum with selected age group: 5-14, 15-24, 25-34, 35-44, 45-54.

ageg<-c(5,15, 25, 35, 45, Inf)
Mort$AgeG<-cut(Mort$edad, ageg, include.lowest=TRUE)
namesageg<-names(table(Mort$AgeG))

## Create a collum with a code to identify age-sex 

Mort$AgeSexG<-as.factor(100*(3-as.numeric(Mort$sexo))+ as.numeric(Mort$AgeG))

## Save prepared data set for analysis

save.image("C:\Program Files\eclipse\workspace\Spatial_Stat\Images\\Mort_89_08.RData")