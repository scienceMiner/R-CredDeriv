implied_corrt <- function(mktSpread, lower, upper, initialCorr=0.3)
{

	ACCURACY <- 1.0e-4
	MAX_ITER <- 100
	HIGH_VALUE <- 1
	ERROR <- -1e40

	corr_low <- 1e-4
	corr_high <- initialCorr
	
	#lowSpread <- getSpreadt(corr_low,lower,upper)
	highSpread <- getSpreadt(corr_high,lower,upper)

	cat("high Spread ", highSpread, " has correlation: ", corr_high, " \n" )

	for (i in 0:MAX_ITER)
	{
			
		corr <- ((corr_low + corr_high) / 2)
		cat ("corr is " , corr, " \n " )
		cat ("corr low is " , corr_low, " \n " )
		cat ("corr high is " , corr_high, " \n " )

		midSpread <- getSpreadt(corr,lower,upper)
		
		cat("iter mid Spread ", midSpread, " has correlation: ", corr, "\n")

		test <- midSpread-mktSpread
		if (abs(test) < ACCURACY)
		{
			return(corr)
		}

		if (midSpread < mktSpread && midSpread < highSpread) 
		{ 
			corr_low <- corr 
			print('test is less than 0')
		}
		else if (midSpread > mktSpread && midSpread > highSpread) 
		{ 
			corr_low <- corr 
			print('test is less than 0')
		}
		else
		{
		  	corr_high <- corr
			print('test is greater than 0')

		}

	}

	return(ERROR)
}


getSpreadt <- function(correl,trancheLow,trancheHigh)
{
	# default intensity is set to 0.01 by default
	return(CDOPricet(100,0.4,0.01,correl,1,1,trancheLow,trancheHigh,0.04,10000,1))
}



repmat <- function(a,n,m) {kronecker(matrix(1,n,m),a)}

CDOPricet <- function(N,R,lambdaf,rho,n,c,a,d,r,No,flag)  
{  

 TM <- 5                     # length of the CDO     
 tstep <- 0.5                # the coupon payments     
 nn  = n * N                 # the total notional     
 loss = n * ( 1 - R)         # the total loss where R is recovery Rate
 T <- seq(0,TM,tstep)        # vector for the fixed coupon dates     
 Tmod <- t(array(rep(T,N), dim=c(length(T),N)))          # matrix of fixed coupon dates for all N companies     
 discount <- exp(-r*T[2:length(T)])   # discounted fixed coupon vector     

# just to initialize the generator     

 MRho <- repmat(rho,N,N)      # initializing the correlation matrix     
 for (i in 1:length(MRho[1,])) { MRho[i,i] <- 1 }          # filling diagonal entries with 1      
 #n<-c(N)
 #I <- matrix(rho,nrow=n,ncol=n)
 #MRho[row(MRho)==col(MRho)] <- 1
    
 MRho <- chol(MRho)          # doing the Cholesky factorization     
 fixedtot <- 0                # initializing for fixed leg total     
 floattot <- 0                # initializing for floating leg total     
 sqfixtot <- 0                # for standard error estimate     
 sqfltot  <- 0                # for standard error estimate   

 Values <- N * No
 PMat <- array(rt(Values,12),dim=c(N,No)) # N rows, No columns
  
 PMat1 <- t(MRho) %*% PMat         # to get the correlated Gaussian matrix  
   
 PMat11 <- pt(PMat1,12) # take the CDF to make them a copula     

 PMat2 <- -log(1 - PMat11)/ lambdaf    # inverse function to get the default time    

# Begin MC simulations 
 for (i in 1:No)                            # loop for different paths of MC 
{       

 PMat3 <- PMat2[,i]                # getting the i'th path        
 Pmatmod <-repmat(PMat3,1,2*TM+1)  # getting it for the fixed coupon dates        
 Temp1 <- Pmatmod < Tmod           # keeping default times that are only within the CDS maturity        
 Lmat <- loss*Temp1                # getting the losses matrix by multiplying with the defaults            
 Tloss <- colSums(Lmat)                # summing up the losses         
 Ploss <- Tloss/nn                  # getting the loss percentages  
 PlossSum <- rep(0,length(Tloss))
 for (j in 1:length(Ploss))
	{ PlossSum[j] <- max(Ploss[j]-a,0) - max(Ploss[j]-d,0)        }
 #Plossum <- max(Ploss-a,0)-max(Ploss-d,0)  # getting the loss percentage in the tranche         
 Lossum <- PlossSum*nn                        # getting the absolute loss in the tranche         
 tempplos <- PlossSum[2:length(PlossSum)]-PlossSum[1:length(PlossSum)-1] #          
 temp2 <- nn * tempplos         
 temp <- nn*(d-a-PlossSum)                        # getting the notional left in the tranche                
 coupon <- tstep*(temp[1:length(temp)-1] + temp[2:length(temp)])/2 # getting the fixed coupons 
        
 fl_flows <- discount*temp2                      # getting the discounted floating flows         
 fx_flows <- discount*coupon*c                  # getting the discounted fixed flows         

 Vfloat <- sum(fl_flows)                          # the total floating flows for this path         
 floattot <- floattot + Vfloat                  # the total floating flows until now         
 sqfltot  <- sqfltot  + (Vfloat^2)              # to get the standard error square term         
 Vfixed <- sum(fx_flows)                          # the total fixed flows for this path         
 fixedtot <- fixedtot + Vfixed                  # the total fixed flows until now         
 sqfixtot <- sqfixtot + (Vfixed^2)              # to get the standard error square term     

}    

# End MC simulations



result <- 0
#disp('Monte Carlo fixed leg value estimate:')    
result[1] <- fixedtot/No    
#disp('Monte Carlo floating leg value estimate:')    
result[2] <- floattot/No    
#disp('Monte Carlo CDO Value estimate:')    
#if flag == 1                 # if flag is set the buyer's value is displayed         
result[3] <- (floattot - fixedtot)/No    
#else         
#result(1,3) <- (fixedtot - floattot)/No          # if flag is not set the seller's value is displayed   
#end    
#}

#disp('Monte Carlo Breakeven Spread estimate:')    
result[4] <- floattot/(fixedtot/c)    
#disp('Monte Carlo Fixed leg standard error:')    
result[5] <-  (1/No) * sqrt(sqfixtot - ((1/No) * (fixedtot^2)))    
#disp('Monte Carlo Floating leg standard error:')    
result[6] <- (1/No) * sqrt(sqfltot - ((1/No) * (floattot^2)))     

# return the breakeven spread ALONE for simulation purposes
return(result[4])

}



imp_correlationMain <- function()
{
	EqTrancheCorr <- implied_corrt(0.1396,0.00,0.03,initialCorr=0.7)

	MezzTrancheCorr <- implied_corrt(0.0351,0.03,0.06,initialCorr=0.7)

	SenTrancheCorr <- implied_corrt(0.0215,0.06,0.09,initialCorr=0.7)

	SupSenTrancheCorr <- implied_corrt(0.0136,0.09,0.12,initialCorr=0.8)

	SupSupSenTrancheCorr <- implied_corrt(0.0066,0.12,0.22,initialCorr=0.8)

}







