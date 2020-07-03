  
#Calculates spread of nth to default swap using procedure mentioned  
#in Appendix A of paper 
#Valuation of a CDO and an nth to Default CDS Without Monte Carlo Simulation   
# Calculation of the Probability Distribution of the Time of the nth Default 
# which uses a recurrence relationship.


getBasketSpread(0.01,10,1,0.3,0.4,5)
getBasketSpread(0.01,10,2,0.3,0.4,5)
getBasketSpread(0.01,10,3,0.3,0.4,5)
getBasketSpread(0.01,10,4,0.3,0.4,5)
getBasketSpread(0.01,10,5,0.3,0.4,5)

getBasketSpread <- function(intensity,obligors,seniority,corr,rec,maturity)
{

#--------- Parameters  ---------- 
lambda<-intensity  	#Default intensity for all firms 
N<-obligors  		#No of obligors 
k<-seniority  		#seniority level eg 2nd to default swap 
rho<-corr  			#correlation between each pair of entities 
T <- maturity  		#maturity of default swap 
r<-0.05  			#risk free rate  
Recovery_rate<-rec  	#recovery rate 
delta <- 0.5  		#semi-annual fixing 
ntimesubsteps<-4  	#no. of sub timesubsteps within each delta for int 
#---------------------------------------   


bdebug<-0  
n<-T/delta  #time steps for indexing preminum payments 
dt<-delta  
dt2<-dt/ntimesubsteps   

tmpintegrand <- function(M) 	
{

	ai<-rho^0.5  	
	Qi<-1-exp(-lambda*g_t)	# Follow a Poisson process
	Fi<-qt(Qi,5)  	
	ai2<-(1-ai*ai)^.5  
	#tadjust<-(rchisq(1,5)/5)^0.5
	K <- M *sqrt(0.6)
	#K <- M
	tmp<-(Fi-ai*(K))/ai2  	
	Si<-(1-pt(tmp,5))  	#probability of survival of each firm 	
	piT0<-Si^N  		#probability that all firms will survive 
	wi<-(1-Si)/Si
	 	
	Vvec<-array(c(rep(wi,N)),dim = c(1,N))

	idxvec<-array(c(1:N),dim = c(1,N))
	Vvec<-N*(Vvec^idxvec)  	
	Uvec<-array(c(rep(0,N)),dim = c(1,N))

	Uvec[1]<-Vvec[1]  	
	piTvec<-array(c(rep(0,N)),dim = c(1,N))  	

	for(ki in 2:N)
	{ 		
		tmpsum<-0  		
		for(ki2 in 1:(ki-1)) 	
		{		
			tmpsum <- tmpsum-(-1)^(ki2)*Uvec[ki-ki2]*Vvec[ki2]
		}
	
		tmpsum <- tmpsum+(-1)^(ki+1)*Vvec[ki]	
		Uvec[ki] <- (tmpsum/ki)
	}

	piTvec<-outer(Uvec,piT0,"*")
	
	survival_prob<-1-sum(piTvec[k:N]) 
	
	return(survival_prob*dt(K,5))

}  

if (bdebug == 0)	
{
	SurvivalProbMat<-array(0,dim=c(ntimesubsteps*n+1,2))	
	t1<-0
	for(i in 1:length(SurvivalProbMat[,1]))
	{ 		
		g_t<-t1  		
		SurvivalProbMat[i,1]<-t1 
		result<-integrate(Vectorize(tmpintegrand),-10,10)

		numRes<-as.numeric(result[1])
		SurvivalProbMat[i,2] <- numRes
		t1<-(t1+dt2)  	
	} 
}

#SurvivalProbMat  
DP<-0  #Expected value of average default leg payments 
PL<-0  #Expected value of average Premium leg payments 
AP<-0  #Expected value of average Accrued premium  

for (i in 2:length(SurvivalProbMat[,1]))
{ 	
	t<-SurvivalProbMat[i,1] 	
	B<-exp(-r*t)  	
	defaultProb<-(1-SurvivalProbMat[i,2])-(1-SurvivalProbMat[i-1,2])  	
	DP<-DP+(1-Recovery_rate)*B*defaultProb  
	if ((t%%delta) < 0.01) 		
	{
		PL<-PL+delta*B*SurvivalProbMat[i,2]  		
		AP<-AP+delta*B*defaultProb  			
	}
	else if (t > delta)
	{
		tgap<-t%%delta 		
		AP<-AP+tgap*B*defaultProb  	
	}
}

#DP 
#PL 
#AP  

spread<-10000*DP/(PL+AP)
return(spread)

}



getBasketSpreadMatrix <- function(matri,c1,c2)
{
	rec=matri[c2];
	correl=matri[c1];
	return(getBasketSpread(0.01,10,6,correl,rec,5))

}


recovery <- seq(0.05,0.95,by=0.05)
correlation <- seq(0.05,0.5,by=0.025)

t1 <- rep(recovery,19)
s1 <- rep(correlation,each=19)

m1 <- cbind(t1,s1)

spreads <- apply(m1,1, getBasketSpreadMatrix, c1="s1", c2="t1" )


trellis.par.set("axis.line", list(col="transparent"),
	clip = list(panel = "off"))

trellis.par.set(theme = col.whitebg())
poly.border<-trellis.par.get("box.rectangle")
poly.border$border<-"green"
trellis.par.set("box.rectangle",poly.border)

fig5 <- wireframe(spreads ~ t1 * s1, 
	scales = list(arrows=FALSE, cex= .55, col = "black", font = 3), 
	drape = TRUE,
	screen = list(z = -75, x = -75),
	aspect = c(1,1),
	light.source = c(100,0,50),
	colorkey = FALSE,
	xlab = expression(paste(Recovery)),
	ylab = expression(paste(Correlation)),
	zlab = list(label = "Breakeven Spread", rot = 90),
	split = c(1,1,2,1),
	zlim = range(seq(0,25,0.1))
)

fig5








