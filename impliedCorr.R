repmat <- function(a,n,m) {kronecker(matrix(1,n,m),a)}

CDOPrice <- function(N,R,lambdaf,rho,n,c,a,d,r,No,flag)  
{  
 #local definitions
 # N = 100
 # R = 0.4
 #lambdaf = 0.015
 #rho = 0.1
 #n = 1
 #c = 1
 #a = 0.0
 #d = 0.03
 #r = 0.04
 #No = 10000
 #flag = 1
 ## end local
 # 100,rec,0.015,correl,1,1,0.0,0.03,0.04,sims,1

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
 PMat <- array(rnorm(Values),dim=c(N,No)) # N rows, No columns
  
 PMat1 <- t(MRho) %*% PMat         # to get the correlated Gaussian matrix  
   
 PMat11 <- pnorm(PMat1) # take the CDF to make them a copula     

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



getImpliedCorr <- function(correl,mktSpread,trancheLow,trancheHigh)
{
	# default intensity is set to 0.01 by default
	return(CDOPrice(100,0.4,0.01,correl,1,1,trancheLow,trancheHigh,0.04,10000,1)-mktSpread)
}


getSpread <- function(correl,trancheLow,trancheHigh)
{
	# default intensity is set to 0.01 by default
	return(CDOPrice(125,0.4,0.01,correl,1,1,trancheLow,trancheHigh,0.04,10000,1))
}

r4<-manyroots(getSpread,c(0,0.5),trancheLow=0.03,trancheHigh=0.06)$root

r4<-manyroots(getImpliedCorr,c(0,0.5),mktSpread=0.0351,trancheLow=0.03,trancheHigh=0.06)$root


implied_correlation <- function(mktSpread, lower, upper, initialCorr=0.3)
{

	ACCURACY <- 1.0e-3
	MAX_ITER <- 100
	HIGH_VALUE <- 1
	ERROR <- -1e40

	corr_low <- 1e-4
	corr_high <- initialCorr
	
	spread <- getSpread(corr_high,lower,upper)

	cat("Spread ", spread, " has correlation: ", corr_high, " \n" )

	while (spread < mktSpread)
	{
		orig <- corr_high
		corr_high <- 0.5 * corr_high
		if (corr_high > 0.9999)
		{
			corr_high = orig + 0.05
		}

		spread <- getSpread(corr_high,lower,upper)
		
		cat("while Spread ", spread, " has correlation: ", corr_high, "\n")

		if (corr_high > HIGH_VALUE) return(ERROR)

	}

	test <- 0
	for (i in 0:MAX_ITER)
	{
			
		corr <- ((corr_low + corr_high) / 0.5)
	
		spread <- getSpread(corr,lower,upper)
		
		cat("iter Spread ", spread, " has correlation: ", corr, "\n")

		test <- spread-mktSpread
		if (abs(test) < ACCURACY)
		{
			return(corr)
		}

		if (test < 0.0) 
		{ 
			corr_high <- corr 
			print('test is less than 0')
		}
		else
		{
		  	corr_low <- corr
			print('test is greater than 0')

		}

	}

	return(ERROR)
}



implied_corr2 <- function(mktSpread, lower, upper, initialCorr=0.3)
{

	ACCURACY <- 1.0e-4
	MAX_ITER <- 100
	HIGH_VALUE <- 1
	ERROR <- -1e40

	corr_low <- 1e-4
	corr_high <- initialCorr
	
	#lowSpread <- getSpread(corr_low,lower,upper)
	highSpread <- getSpread(corr_high,lower,upper)

	cat("high Spread ", highSpread, " has correlation: ", corr_high, " \n" )

	for (i in 0:MAX_ITER)
	{
			
		corr <- ((corr_low + corr_high) / 2)
		cat ("corr is " , corr, " \n " )
		cat ("corr low is " , corr_low, " \n " )
		cat ("corr high is " , corr_high, " \n " )

		midSpread <- getSpread(corr,lower,upper)
		
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




base_correlation <- function(mktSpread, prevTrancheCorr, upper, initialCorr=0.3)
{
	# No need to lower tranche information here 
	# as it is always 0

	# bootstrap step
	# the keySpread is fixed from the previous tranche
	prevTranchePV <- getCDOValue(mktSpread,prevTrancheCorr,upper)

	ACCURACY <- 1.0e-4
	MAX_ITER <- 100
	HIGH_VALUE <- 1
	ERROR <- -1e40
	lower <- 0.00

	corr_low <- 1e-4
	corr_high <- initialCorr
	
	highSpread <- getBaseSpread(prevTranchePV,corr_high,upper)

	cat("high Spread ", highSpread, " has correlation: ", corr_high, " \n" )

	for (i in 0:MAX_ITER)
	{
			
		corr <- ((corr_low + corr_high) / 2)
		cat ("corr is " , corr, " \n " )
		cat ("corr low is " , corr_low, " \n " )
		cat ("corr high is " , corr_high, " \n " )

		midSpread <- getBaseSpread(prevTranchePV,corr,upper)
		
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



base_correlationMain <- function()
{
	EqTrancheCorr <- implied_corr2(0.1396,0.00,0.03,initialCorr=0.5)

	MezzTrancheCorr <- base_correlation(0.0351,EqTrancheCorr,0.06,initialCorr=0.45)

	SenTrancheCorr <- base_correlation(0.0215,MezzTrancheCorr,0.09,initialCorr=0.5)

	SupSenTrancheCorr <- base_correlation(0.0136,SenTrancheCorr,0.12,initialCorr=0.8)

	SupSupSenTrancheCorr <- base_correlation(0.0066,SupSenTrancheCorr,0.22,initialCorr=0.8)

}

imp_correlationMain <- function()
{
	EqTrancheCorr <- implied_corr2(0.1396,0.00,0.03,initialCorr=0.7)

	MezzTrancheCorr <- implied_corr2(0.0351,0.03,0.06,initialCorr=0.2)

	SenTrancheCorr <- implied_corr2(0.0215,0.06,0.09,initialCorr=0.3)

	SupSenTrancheCorr <- implied_corr2(0.0136,0.09,0.12,initialCorr=0.8)

	SupSupSenTrancheCorr <- implied_corr2(0.0066,0.12,0.22,initialCorr=0.8)

}










lower <- 0.00
upper <- 0.03

mktSpread <- 0.1396
impliedCorr <- implied_corr2(mktSpread,lower,upper,initialCorr=0.8)




lower <- 0.09
upper <- 0.12

mktSpread <- 0.0136
impliedCorr <- implied_corr2(mktSpread,lower,upper,initialCorr=0.5)

print('Implied Correlation:')    
print(impliedCorr)


lower <- 0.12
upper <- 0.22

mktSpread <- 0.0066
impliedCorr <- implied_corr2(mktSpread,lower,upper,initialCorr=0.5)

print('Implied Correlation:')    
print(impliedCorr)




lower <- 0.03
upper <- 0.06

mktSpread <- 0.0351
impliedCorr <- implied_correlation(mktSpread,lower,upper,initialCorr=0.3)

print('Implied Correlation:')    
print(impliedCorr)




lower <- 0.06
upper <- 0.09

mktSpread <- 0.0215
impliedCorr <- implied_correlation(mktSpread,lower,upper,initialCorr=0.3)

print('Implied Correlation:')    
print(impliedCorr)


lower <- 0.09
upper <- 0.12

mktSpread <- 0.0136
impliedCorr <- implied_correlation(mktSpread,lower,upper,initialCorr=0.3)

print('Implied Correlation:')    
print(impliedCorr)



lower <- 0.12
upper <- 0.22

mktSpread <- 0.0066
impliedCorr <- implied_correlation(mktSpread,lower,upper,initialCorr=0.3)

print('Implied Correlation:')    
print(impliedCorr)






CDOPrice(100,0.4,0.01,0.65,1,1,0.00,0.03,0.04,10000,1)


#
# t1<-getSpread(0.3,0.00,0.03)
# t1


factor <- seq(0.1,0.9,by=0.05)
tlgd <- seq(0.1,0.9,by=0.05)

t1 <- rep(factor,1)
s1 <- rep(tlgd, 1)

m1 <- cbind(t1,s1)

spreads03 <- apply(m1,1, getSpread2t, c1="s1", 0.0, 0.03 )
spreads03

spreads36 <- apply(m1,1, getSpread2t, c1="s1", 0.03, 0.06 )
spreads36

spreads69 <- apply(m1,1, getSpread2t, c1="s1", 0.06, 0.09 )
spreads69

spreads912 <- apply(m1,1, getSpread2t, c1="s1", 0.09, 0.12 )
spreads912

spreads1222 <- apply(m1,1, getSpread2t, c1="s1", 0.12, 0.22 )
spreads1222



tranches <- 5

# get the range for the x and y axis
yrange <- range(0,0.05)
y2range <- range(0,0.25)
xrange <- range(0.05,0.9)

# set up the plot
plot(xrange, yrange, axes="T",type="n", xlab="Correlation",ylab="Breakeven Spread" )
colors <- rainbow(tranches)
linetype <- c(1:tranches)
plotchar <- seq(18,18+tranches,1)
#plotchar <- c(1:tranches)


lines(s1, spreads03, type="b", lwd=1.5,lty=linetype[1], col=colors[1],pch=plotchar[1])
lines(s1, spreads36, type="b", lwd=1.5,lty=linetype[2], col=colors[2],pch=plotchar[2])
lines(s1, spreads69, type="b", lwd=1.5,lty=linetype[3], col=colors[3],pch=plotchar[3])
lines(s1, spreads912, type="b", lwd=1.5,lty=linetype[4], col=colors[4],pch=plotchar[4])
lines(s1, spreads1222, type="b", lwd=1.5,lty=linetype[5], col=colors[5],pch=plotchar[5])
# add a title and subtitle
title("CDO Tranche Spreads", "using Student-t distribution")

# add a legend
leg.txt <- c("Equity (0-3%)", "Junior Mezz (3-6%)","Mezz (6-9%)", "Sub Senior (9-12%)", "Senior (12-22%)")
legend(0.6, 0.25, leg.txt, cex=0.8, col=colors,pch=plotchar, lty=linetype, title="Tranche")

# end 

getSpread2 <- function(matri,c1,trancheLow,trancheHigh)
{
	correl=matri[c1];
	cat("correlation is " , correl , " \n " )
	# default intensity is set to 0.01 by default
	return(c(CDOPrice(100,0.4,0.01,correl,1,1,trancheLow,trancheHigh,0.04,10000,1)))
}


getSpread2t <- function(matri,c1,trancheLow,trancheHigh)
{
	correl=matri[c1];
	cat("correlation is " , correl , " \n " )
	# default intensity is set to 0.01 by default
	return(c(CDOPricet(100,0.4,0.01,correl,1,1,trancheLow,trancheHigh,0.04,10000,1)))
}


plot(factor, spreads, type = "l")


