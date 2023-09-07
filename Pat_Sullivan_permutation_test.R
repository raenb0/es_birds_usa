#
# Exploring permutation test, binomial t-test, and chisq contigency table
# Sullivan 8/23/2023
#
# Simple simulation
p1 = 0.51  # Probability of a bird in carbon area
p2 = 0.49  # Probability of a bird in noncarbon area
n1 = 4400  # Number of carbon areas
n2 = 5600  # Number of noncarbon areas
avgB = 100 # Average number of birds per area over all
#
xYC = rbinom(n1,avgB,p1)  # Bird count by area in carbon areas
xNC = rbinom(n2,avgB,p2)  # Bird count by area in noncarbon areas
#
xYCp = xYC/n1  # Percent bird by area in carbon areas
xNCp = xNC/n2  # Percent bird by area in noncarbon areas
#
sum(xYCp)  # Estimated percent of birds in total carbon area under simulation
sum(xNCp)  # Estimated percent of birds in total noncarbon area under simulation
# 
# Permutation Test
#
percnts = NULL
for(i in seq(1000)){
   xx = sum(sample(c(xYCp,xNCp),n1))
   percnts = c(percnts,xx)
}
mean(percnts) # mean total percent birds in carbond area under null hypothesis that mean=n1/(n1+n2)
hist(percnts) # distribution of total percent birds in carbon area under null hypothesis
#
# 95% confidence interval of total percent birds in carbon area under null hypothesis
#
round(c(quantile(percnts,0.025),quantile(percnts,0.975)),3) #Does 95% CI contain sum(xYCp)~p1
# compare with
100*p1
#
#
# Probability of getting p1 by chance
#
(1-length(percnts[percnts>p1])/length(percnts) )/2 # For 2-sided test
#
#---------------------------------------------------------------------------------------
#
# Using two-sample t-test on proportions
#
# https://vitalflux.com/two-sample-z-test-for-proportions-formula-examples/
# This is testing if p1 is not equal to p2 (slightly different test) 
# This results in a Z score, recall statistically significant if abs(Z) > 2
#
phat = (p1*n1+p2*n2)/(n1+n2)
phat
Z = (p1-p2)/sqrt(phat*(1-phat)*((1/n1)+(1/n2))) # Z score
abs(Z) 
1-pnorm(abs(Z),0.975) # significant if less that 0.05
#
#---------------------------------------------------------------------------------------
#
# Using a contingency table with a chi-square test
#
tab1 = data.frame(YC=c(sum(xYC),n1),NC=c(sum(xNC),n2))
Obs = c(n1*p1,n2*p2)
Exp = sum(Obs) * c(n1/(n1+n2), n2/(n1+n2))
#
chi.square = sum((Obs - Exp)^2/Exp)
chi.square
#
df.test = length(Obs) - 1
df.test
#
p.chisq.test = 1 - pchisq(chi.square, df.test)
p.chisq.test  # significant if less that 0.05



