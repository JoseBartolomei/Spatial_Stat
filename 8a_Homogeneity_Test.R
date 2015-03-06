# TODO: Add comment
# 
# Author: jbartolomei
###############################################################################
library(DCluster)
##  Chi 2 test for overdispertion
chtest<-as.list(rep(NA,4))

for (i in 1:4)

{
	chtest[[i]]<-achisq.test(Observed ~ offset(log(Expected)), data=data4DC.P[[i]], "negbin", R=999)

	print("P value - Chi square test for overdispertion")
	
	print(1 - pchisq(chtest[[i]]$t0, 78 - 1))
}

## Pottwhoff-Whittinghill’s test for overdispertion
pwtest<-as.list(rep(NA,4))
Oplus<-as.list(rep(NA,4))

for (i in 1:4)
{
	pwtest[[i]]<-pottwhitt.test(Observed ~ offset(log(Expected)), data=data4DC.P[[i]], "negbin", R=200)

	Oplus[[i]]<-sum(data4DC.P[[i]]$Observed)

	print("P value - Pottwhoff-Whittinghill’s test for overdispertion for each period")
	print(1 - pnorm(pwtest[[i]]$t0, Oplus[[i]] * (Oplus[[i]] - 1), sqrt(2 * 78 * Oplus[[i]] * (Oplus[[i]] - 1))))
}

## Dean test for overdispertion
x.glm<-as.list(rep(NA,4))

for (i in 1:4)
{
	x.glm[[i]]<-glm(Observed ~ 1+offset(log(Expected)), data=data4DC.P[[1]], family=poisson())
	
	print("P value - Pottwhoff-Whittinghill’s test for overdispertion for each period")
	
	print(c(DeanB(x.glm[[i]])$p.value, DeanB2(x.glm[[i]])$p.value))
}
