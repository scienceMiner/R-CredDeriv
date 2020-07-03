  
#Calculates spread of nth to default swap using procedure mentioned  
#in Appendix A of paper 
#Valuation of a CDO and an nth to Default CDS Without Monte Carlo Simulation   
# Calculation of the Probability Distribution of the Time of the nth Default 
# which uses a recurrence relationship.



rinv <- function(x)
{
	return(x^(1/x))
}

a<-seq(0,10,0.01)
r<-seq(0,10,0.01)

for (i in 1:1000)
{
	r[i]<-rinv(i)
}

plot(a,r,xlim=c(0,10),ylim=c(0,2),pch=0)
myline.fit <- lm(a ~ r) 






