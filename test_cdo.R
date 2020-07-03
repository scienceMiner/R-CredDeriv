  
#Calculates spread of nth to default swap using procedure mentioned  
#in Appendix A of paper 
#Valuation of a CDO and an nth to Default CDS Without Monte Carlo Simulation   
# Calculation of the Probability Distribution of the Time of the nth Default 
# which uses a recurrence relationship.


#--------- Parameters  ---------- 
lambda<-0.01  		#Default intensity for all firms 
N<-100  			#No of obligors 
rho<-0.3  			#correlation between each pair of entities 
T <- 5  			#maturity of default swap 
r<-0.05  			#risk free rate  
Recovery_rate<-0.4  	#recovery rate 
delta <- 0.5  		#semi-annual fixing 
ntimesubsteps<-2  	#no. of sub timesubsteps within each delta for int 
lower<-0.03			#lower bound
upper<-0.06			#upper bound
#---------------------------------------   


bdebug<-0  
n<-T/delta  #time steps for indexing preminum payments 
dt<-delta  
dt2<-dt/ntimesubsteps   

getSurvivalProbs <- function(M) 	
{

	ai<-rho^0.5  	
	Qi<-1-exp(-lambda*g_t)	# Follow a Poisson process
	Fi<-qnorm(Qi)  	
	ai2<-(1-ai*ai)^.5  
	tmp<-(Fi-ai*(M))/ai2  	
	Si<-(1-pnorm(tmp))  	#probability of survival of each firm 	
	piT0<-Si^N  		#probability that all firms will survive 
	wi<-(1-Si)/Si
	 	
	Vvec<-array(c(rep(wi,N)),dim  <- c(1,N))

	idxvec<-array(c(1:N),dim  <- c(1,N))
	Vvec<-N*(Vvec^idxvec)  	
	Uvec<-array(c(rep(0,N)),dim  <- c(1,N))

	Uvec[1]<-Vvec[1]  	
	piTvec<-array(c(rep(0,N)),dim  <- c(1,N))  	

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
	
	print(piTvec)
	survival_prob<-1-sum(piTvec[k:N]) 
	
	return(survival_prob*dnorm(M))

}  


getDefaultProbs <- function(M,g_t) 	
{

	ai<-rho^0.5  	
	Qi<-1-exp(-lambda*g_t)	# Follow a Poisson process
	Fi<-qnorm(Qi)  	
	ai2<-(1-ai*ai)^.5  
	tmp<-(Fi-ai*(M))/ai2  	
	Si<-(1-pnorm(tmp))  	#probability of survival of each firm 	
	piT0<-Si^N  		#probability that all firms will survive 
	wi<-(1-Si)/Si
	#print(wi)
	#print(" tmp ")
	#print(tmp)
	#print("Si ")
	#print(Si)
	#print(" Fi ")
	#print(Fi)
	#print(" Qi ")
	#print(Qi)
 	
	Vvec<-array(c(rep(wi,N)),dim  <- c(1,N))

	idxvec<-array(c(1:N),dim  <- c(1,N))
	Vvec<-N*(Vvec^idxvec)  	
	Uvec<-array(c(rep(0,N)),dim  <- c(1,N))

	Uvec[1]<-Vvec[1]  	
	piTvec<-array(c(rep(0,N)),dim  <- c(1,N))  	

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
	
#	print("No default")
#	print(piT0)
	#default_prob<-sum(piTvec[k:N]) 
	
	#return(default_prob*dnorm(M))

	#piTvec <- rbind(piT0, piTvec)
	piTvec<-append(piTvec, piT0, after=0)

	return(piTvec)

}  

getDefaultMatrix<-function(M)
{
	g_dt <- dt2

	DefProbMat<-array(0,dim=c(ntimesubsteps*n+1,N+1))

	t1<-0
	for(i in 1:(length(DefProbMat[,1])))
	{ 		
		g_t<-t1
		#print("g_t is now")
		#print(g_t)
		defProbVec<-getDefaultProbs(M,g_t) 
		#print(defProbVec)

		#DefProbMat[i]<-defProbVec

		for (j in 0:length(defProbVec))
		{
			DefProbMat[i,j]<-defProbVec[j]
 		}
		t1<-(t1+dt2)  	
	} 

	return(DefProbMat)
}



# integrates conditional default payments for gaussian distribution
getExpectedDefaultPayments<-function()
{
	return(integrate(Vectorize(getConditionalDefaultPayment),-4,4))
}

dprob<- 0

getConditionalDefaultPayment <- function(M)
{

	defProbMatrix<-getDefaultMatrix(M)

	#print(defProbMatrix)
	
	t<-0
	#DP1<-0.0
	survival_prob<-0.0
	dprob<-0.0

	#assume all firms have equal notional
	#need to find defaults whose losses lie in this tranche

	lowerLimit<-lower*N
	upperLimit<-upper*N
	defaultLoss<-1-Recovery_rate 							# default loss of each firm

	firstDefaultFirm <-ceiling(lowerLimit/defaultLoss)
	firstFirmAccumulatedLoss <-(1-Recovery_rate)*firstDefaultFirm	#first def loss in tranche
	firstFirmLoss <-firstFirmAccumulatedLoss-lowerLimit 			#firm default loss portion in tranche

	lastDefaultFirm <-floor(upperLimit/defaultLoss)				#last firm with def loss in this tranche
	lastFirmAccumulatedLoss <-(1-Recovery_rate)*lastDefaultFirm		# last firm acc loss
	lastFirmLoss <- upperLimit-lastFirmAccumulatedLoss 			#default loss portion in tranche	

#	print('FirstDefaultFirm')
#	print(firstDefaultFirm)

#	print('lastDefaultFirm')
#	print(lastDefaultFirm)

	for (i in 1:length(defProbMatrix[,1]))
	{
		t <-t + dt 
		B <-exp(-r*t) 
		#get probability that defaults occur between t and t+dt time interval
		tmpsum<-0
		accumulatedLoss <- firstFirmLoss 		#loss beared by this tranche

		tmpsum <- accumulatedLoss * ( defProbMatrix[i,firstDefaultFirm]- defProbMatrix[i-1,firstDefaultFirm] )
		
		for (j in (firstDefaultFirm+1):lastDefaultFirm)
		{
			accumulatedLoss <- accumulatedLoss + defaultLoss 
			tmpsum <- tmpsum + accumulatedLoss*(defProbMatrix[i,j]-defProbMatrix[i-1,j]) 
		}

		accumulatedLoss<- accumulatedLoss + lastFirmLoss 
		tmpsum <-tmpsum + accumulatedLoss*(defProbMatrix[i,lastDefaultFirm+1] - defProbMatrix[i-1,lastDefaultFirm+1])

		for (j in (lastDefaultFirm+2):N)
		{
			tmpsum <- tmpsum + accumulatedLoss * (defProbMatrix[i,j] - defProbMatrix[i-1,j]) 
		}
		
		dprob <- (B*tmpsum) + sum(dprob) # why do we need this sum statement?
		
	}
	
	return(dprob*dnorm(M)) 
	
}


getConditionalPremiumPayment <- function(M)
{
	defProbMatrix <-getDefaultMatrix(M)

#	print(defProbMatrix)
	t<-0
	lowerLimit<-lower*N
	upperLimit<-upper*N
	interval<-upperLimit-lowerLimit
	defaultLoss<-1-Recovery_rate		#default loss of each firm

	#first firm whose default starts bringing loss in this tranche
	firstDefaultFirm<-ceiling(lowerLimit/defaultLoss)
	firstFirmAccumulatedLoss<-(1-Recovery_rate)*firstDefaultFirm
	firstFirmLoss<-firstFirmAccumulatedLoss-lowerLimit

	#last firm whose default brings loss in this tranche
	lastDefaultFirm<-floor(upperLimit/defaultLoss) 
	lastFirmAccumulatedLoss<-(1-Recovery_rate)*lastDefaultFirm 
	lastFirmLoss<-upperLimit-lastFirmAccumulatedLoss
 
#	print("firstDefaultFirm")
#	print(firstDefaultFirm)
#	print("lastDefaultFirm")
#	print(lastDefaultFirm)
	
	trancheNotional<-upperLimit-lowerLimit 

#	print("trancheNotional")
#	print(trancheNotional)

	PL<-0
	for (i in 1:length(defProbMatrix[,1]))
	{

		t<-t+dt 
		#print("t is ")
		#print(t)
		if ((t %% delta) < 0.0001)
		{
			B<-exp(-r*t) 
		#	print(B)
			tmpsum<-0 
			# interest is to be paid on entire tranche notional until before the first defaulting firm
			for (j in 0:(firstDefaultFirm-1))
			{
				if (i-ntimesubsteps > 0)
				{

					tmpsum <- tmpsum + ((interval)*0.5*(defProbMatrix[i,j]+defProbMatrix[(i-ntimesubsteps),j]) )
				}
				else
				{
					tmpsum <- tmpsum + ((interval)*0.5*(defProbMatrix[i,j]))
				}
				
			}
			accumulatedLoss <- 0
			accumulatedLoss <- accumulatedLoss + firstFirmLoss 
			# remove first firm loss from principal
			if (i-ntimesubsteps > 0)
			{
				tmpsum <- sum(tmpsum)+((interval-accumulatedLoss)*0.5*(defProbMatrix[i,firstDefaultFirm]+defProbMatrix[i-ntimesubsteps,firstDefaultFirm])) 
			}
			else
			{
				tmpsum <- sum(tmpsum)+((interval-accumulatedLoss)*0.5*(defProbMatrix[i,firstDefaultFirm])) 		
			}
		
		#	print("tmpsum interim ")
		#	print(tmpsum)
			

			for (j in (firstDefaultFirm+1):lastDefaultFirm)
			{

				accumulatedLoss <- accumulatedLoss + defaultLoss 
				tmpsum <- tmpsum + (interval-accumulatedLoss)*0.5*(defProbMatrix[i,j]+defProbMatrix[i-ntimesubsteps,j]) 
			}
			PL<-sum(PL)+delta*B*r*sum(tmpsum) 
		#	print("tmpsum")
		#	print(tmpsum)
		#	print("PL")
		#	print(PL)
		}
	}
	 
	return(PL*dnorm(M))
}

# integrates conditional default payments for gaussian distribution
getExpectedPremiumPayments<-function()
{
	return(integrate(Vectorize(getConditionalPremiumPayment),-10,10))
}


#r3<-getConditionalDefaultPayment(0.15)
#r3

#r4<-getConditionalPremiumPayment(0.15)
#r4

r5<-getExpectedDefaultPayments()
r5
r6<-getExpectedPremiumPayments()
r6

EVcdo <- as.numeric(r5[1])-as.numeric(r6[1])
ECbe <- (10000*as.numeric(r5[1]) / (as.numeric(r6[1]) / r))
	
print("Break Even Spread")
ECbe




