# TODO: Add comment
# 
# Author: jbartolomei
###############################################################################



PR_W1B<-nb2listw(PR_nb1, zero.policy=TRUE, style="B")# B is the basic binary coding
PR_W1W<-nb2listw(PR_nb1, style="W", zero.policy=TRUE )# W is row standardised (sums over all links to n)
PR_W1C<-nb2listw(PR_nb1, zero.policy=TRUE, style="C")# C is globally standardised (sums over all links to n) 
PR_W1U<-nb2listw(PR_nb1, zero.policy=TRUE, style="U")# U is equal to C divided by the number of neighbours (sums over all links to unity)
PR_W1S<-nb2listw(PR_nb1, zero.policy=TRUE, style="S")# while S is the variance-stabilizing coding scheme 
#proposed by Tiefelsdorf et al. 1999, p. 167-168 (sums over all links to n). 


PR_W8B<-nb2listw(PR_nb8D, zero.policy=TRUE, style="B")
PR_W8W<-nb2listw(PR_nb8D, zero.policy=TRUE, style="W")
PR_W8C<-nb2listw(PR_nb8D, zero.policy=TRUE, style="C")
PR_W8U<-nb2listw(PR_nb8D, zero.policy=TRUE, style="U")
PR_W8S<-nb2listw(PR_nb8D, zero.policy=TRUE, style="S")

##  Summary of weights
summary(unlist(PR_W1W$weights))
summary(sapply(PR_W1W$weights, sum))

summary(unlist(PR_W1B$weights))
summary(sapply(PR_W1B$weights, sum))

summary(unlist(PR_W1C$weights))
summary(sapply(PR_W1C$weights, sum))

summary(unlist(PR_W1U$weights))
summary(sapply(PR_W1U$weights, sum))

summary(unlist(PR_W1S$weights))
summary(sapply(PR_W1S$weights, sum))
