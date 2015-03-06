# TODO: Add comment
# 
# Author: jbartolomei
###############################################################################

##  Evaluate over-dispersion comparing mean (Oi) vs var(Oi)

mean(obsMuni.P[[1]])==var(obsMuni.P[[1]])
mean(obsMuni.P[[2]])==var(obsMuni.P[[2]])
mean(obsMuni.P[[3]])==var(obsMuni.P[[3]])
mean(obsMuni.P[[4]])==var(obsMuni.P[[4]])


###   Produce Confidence interval for SMR per period ###

library(epitools)

CISMR.P<-as.list(rep(NA, 4))

for (i in 1:4)
{
	CISMR.P[[i]]<-pois.exact(obsMuni.P[[i]], ExpMuni.P[[i]])
}

par(mfrow=c(2,2))

##  Plot first Period (1989-1993)

plot(1,1, type="n", xlim=c(1,78), ylim=c(0,25),
		main= "Age-sex adjusted SMR and 95% Confidence intervals, 1989-1993", cex.main = 0.6,
		xlab="Municipality", ylab="Relative Risk",cex.lab = 0.6, xaxt="n")
abline(h=1, lty=2)

for(i in 1:78)
{
	if(CISMR.P[[1]]$lower[i]>1 )
	{
		col<-gray(.4)
		lty<-2
		text(i, CISMR.P[[1]]$upper[i]+.31, PR_map$NAME[i],
				srt=90, col=gray(.4), cex=.85)
	}
	else
	{
		col<-"black"
		lty<-1
	}
	
	lines(c(i,i), c(CISMR.P[[1]]$lower[i],CISMR.P[[1]]$upper[i]), col=col, lty=lty)
	points(i, CISMR.P[[1]]$rate[i], pch=18, col=col)
	legend(2, 24, legend="SMR", pch=18, bty="n")
}


##  Plot Second Period (1994-1998)

plot(1,1, type="n", xlim=c(1,78), ylim=c(0,25),
		main= "Age-sex adjusted SMR and 95% Confidence intervals, 1994-1998 ",cex.main = 0.6,
		xlab="Municipality", ylab="Relative Risk", cex.lab = 0.6,  xaxt="n")
abline(h=1, lty=2)

for(i in 1:78)
{
	if(CISMR.P[[2]]$lower[i]>1 )
	{
		col<-gray(.4)
		lty<-2
		text(i, CISMR.P[[2]]$upper[i]+.31, PR_map$NAME[i],
				srt=90, col=gray(.4), cex=.85)
	}
	else
	{
		col<-"black"
		lty<-1
	}
	
	lines(c(i,i), c(CISMR.P[[2]]$lower[i],CISMR.P[[2]]$upper[i]), col=col, lty=lty)
	points(i, CISMR.P[[2]]$rate[i], pch=18, col=col)
	legend(2, 24, legend="SMR", pch=18, bty="n")
}

##  Plot Third Period (1999-2003)

plot(1,1, type="n", xlim=c(1,78), ylim=c(0,25),
		main= "Age-sex adjusted SMR and 95% Confidence intervals, 1999-2003 ", cex.main = 0.6,
		xlab="Municipality", ylab="Relative Risk",cex.lab = 0.6,  xaxt="n")
abline(h=1, lty=2)

for(i in 1:78)
{
	if(CISMR.P[[3]]$lower[i]>1 )
	{
		col<-gray(.4)
		lty<-2
		text(i, CISMR.P[[3]]$upper[i]+.31, PR_map$NAME[i],
				srt=90, col=gray(.4), cex=.85)
	}
	else
	{
		col<-"black"
		lty<-1
	}
	
	lines(c(i,i), c(CISMR.P[[3]]$lower[i],CISMR.P[[3]]$upper[i]), col=col, lty=lty)
	points(i, CISMR.P[[3]]$rate[i], pch=18, col=col)
	legend(2, 24, legend="SMR", pch=18, bty="n")
}

##  Plot Forth Period (2004-2008)

plot(1,1, type="n", xlim=c(1,78), ylim=c(0,25),
		main= "Age-sex adjusted SMR and 95% Confidence intervals, 2004-2008 ", cex.main = 0.6,
		xlab="Municipality", ylab="Relative Risk",cex.lab = 0.6,  xaxt="n")
abline(h=1, lty=2)

for(i in 1:78)
{
	if(CISMR.P[[4]]$lower[i]>1 )
	{
		col<-gray(.4)
		lty<-2
		text(i, CISMR.P[[4]]$upper[i]+.31, PR_map$NAME[i],
				srt=90, col=gray(.4), cex=.85)
	}
	else
	{
		col<-"black"
		lty<-1
	}
	
	lines(c(i,i), c(CISMR.P[[4]]$lower[i],CISMR.P[[4]]$upper[i]), col=col, lty=lty)
	points(i, CISMR.P[[4]]$rate[i], pch=18, col=col)
	legend(2, 24, legend="SMR", pch=18, bty="n")
}

