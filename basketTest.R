  
#Calculates spread of nth to default swap using procedure mentioned  
#in Appendix A of paper 
#Valuation of a CDO and an nth to Default CDS Without Monte Carlo Simulation    

 

#--------- Parameters  ---------- 
lambda<-0.01  		#Default intensity for all firms 
N<-10  			#No of obligors 
k<-2  			#seniority level eg 2nd to default swap 
rho<-0.3  			#correlation between each pair of entities 
T <- 5  			#maturity of default swap 
r<-0.05  			#risk free rate  
Recovery_rate<-0.4  	#recovery rate 
delta <- 0.5  		#semi-annual fixing 
ntimesubsteps<-4  	#no. of sub timesubsteps within each delta for int 
#---------------------------------------   


bdebug<-0  
n<-T/delta  #time steps for indexing preminum payments 
dt<-delta  
dt2<-dt/ntimesubsteps   

M<--0.02
g_t<-5

	ai<-rho^0.5  	
	Qi<-1-exp(-lambda*g_t)
	Fi<-qnorm(Qi)  	
	ai2<-(1-(ai*ai))^.5  
	tmp<-(Fi-(ai*M))/ai2  	
	Si<-(1-pnorm(tmp))  	#probability of survival of each firm 	
	piT0<-Si^N  		#probability that all firms will survive 
	wi<-(1-Si)/Si  	
	Vvec<-t(rep(1,N)*as.vector(wi))
	idxvec<-1:N  	
	idxvec<-t(idxvec)
	Vvec<-N*(Vvec^idxvec)  	
	Uvec<-t(rep(0,N))
	Uvec[1]<-Vvec[1]  	
	piTvec<-t(rep(0,N))  	

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
	piTvec<-piT0*Uvec  	
	survival_prob<-1-sum(piTvec[k:N]) 

	ret1<- survival_prob*dnorm(M)

tmp
Qi
Fi
ai2
Si
wi
Uvec
Vvec
piTvec
survival_prob
ret1

