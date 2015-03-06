# TODO: Add comment
# 
# Author: jbartolomei
###############################################################################

load((file="C:\\Program Files\\eclipse\\workspace\\Spatial_Stat\\Images\\Spatial_Data.RData"))
library (sp)
library(DCluster)
###################################################
### chunk number 2: Empirical Bayes Smoothing
###################################################

## Poisson-Gamma Empirical Bayes Smoothing

pg<-as.list(rep(NA,4))

for (i in 1:4)
{
	pg[[i]]<-empbaysmooth(d[[i]]$Observed, d[[i]]$Expected, maxiter=5)
}

PR.D$PG.P1<-pg[[1]]$smthrr
PR.D$PG.P2<-pg[[2]]$smthrr
PR.D$PG.P3<-pg[[3]]$smthrr
PR.D$PG.P4<-pg[[4]]$smthrr

brks<-exp(quantile(log(c(PR.D$PG.P2)), 0:5/5 ))
brks[1]<-0

atcol<-(0:5)*brks[6]/5

brks<-atcol
colorkey<-list(labels=as.character(c(formatC(brks, format="f", dig=2))),
		at=atcol,  height=.5)

cols <- brewer.pal(6, "Oranges")

print(spplot(PR.D, c("PG.P4", "PG.P3","PG.P2","PG.P1"),  
				main="Age-Sex adjusted SMR smoothed by EB;Poisson-Gamma, 1989-2008", 
				names.attr=c("2004-2008", "1999-2003", "1994-1998", "1989-1993"),
				col.regions=cols, at=brks, axes = TRUE, colorkey=colorkey)) 




## Log normal Empirical Bayes Smoothing

ln<-as.list(rep(NA,4))

for (i in 1:4)
{
	ln[[i]]<-lognormalEB(d[[i]]$Observed, d[[i]]$Expected, maxiter=2000)
}

PR.D$LN.P1<-ln[[1]]$smthrr
PR.D$LN.P2<-ln[[2]]$smthrr
PR.D$LN.P3<-ln[[3]]$smthrr
PR.D$LN.P4<-ln[[4]]$smthrr

brks<-exp(quantile(log(c(PR.D$LN.P1)), 0:5/5 ))
brks[1]<-0

atcol<-(0:5)*brks[6]/5

colorkey<-list(labels=as.character(c(formatC(brks, format="f", dig=2))),
		at=atcol,  height=.5)

print(spplot(PR.D, c("LN.P4","LN.P3","LN.P2","LN.P1"),  
				main="Age-Sex adjusted SMR smoothed by EB;Log-Normal, 1989-2008", 
				names.attr=c("2004-2008", "1999-2003", "1994-1998", "1989-1993"),
				col.regions=cols, at=brks, axes = TRUE, colorkey=colorkey)) 



##Generate table with data
library(xtable)

mysummary<-function (x)
{
	m<-mean(x)
	med<-median(x)
	s.d.<-sd(x)
	min<-min(x)
	max<-max(x)
	
	return(list(Mean = m, Median = med, s.d. = s.d., Min=min, Max=max))
} 

tt<-data.frame(apply(PR.D@data[,c("PG.P1", "PG.P2", "PG.P3", "PG.P4" )], 2, mysummary))
names(tt)<-c("2004-2008", "1999-2003", "1994-1998", "1989-1993")
rownames(tt)<-c("Mean", "Median", "s.d.", "Min", "Max")

xtt<-xtable(tt, label="tab:EB", caption="Summary of values of the EB smoothed risk estimates.")
print(xtt, type="latex")


