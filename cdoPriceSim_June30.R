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

 MRho = repmat(rho,N,N);      # initializing the correlation matrix     
 for (i in 1:length(MRho[1,])) { MRho[i,i] <- 1 }          # filling diagonal entries with 1      
     
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

simCDO <- function(rec,correl,sims,trancheLow,trancheHigh)
{
	return(CDOPrice(100,rec,0.015,correl,1,1,trancheLow,trancheHigh,0.04,sims,1))

}


simCDO2 <- function(matri,c1,c2,sims,trancheLow,trancheHigh)
{
rec=matri[c1];
correl=matri[c2];
	return(c((CDOPrice(100,rec,0.015,correl,1,1,trancheLow,trancheHigh,0.04,sims,1))))

}

recovery <- seq(0.05,0.95,by=0.05)
correlation <- seq(0.05,0.95,by=0.05)

t1 <- rep(recovery,19)
s1 <- rep(correlation,each=19)

m1 <- cbind(t1,s1)

spreads <- apply(m1,1, simCDO2, c1="s1", c2="t1", trancheLow=0.0, trancheHigh=0.03, sims=10000)


trellis.par.set("axis.line", list(col="transparent"),
	clip = list(panel = "off"))

trellis.par.set(theme = col.whitebg())
poly.border<-trellis.par.get("box.rectangle")
poly.border$border<-"green"
trellis.par.set("box.rectangle",poly.border)

fig5 <- wireframe(spreads ~ t1 * s1, 
	scales = list(arrows=FALSE, cex= .55, col = "black", font = 3), 
	drape = TRUE,
	screen = list(z = -50, x = -75),
	aspect = c(1,1),
	light.source = c(100,0,50),
	colorkey = FALSE,
	xlab = expression(paste(Recovery)),
	ylab = expression(paste(Correlation)),
	zlab = list(label = "Breakeven Spread", rot = 90),
	split = c(1,1,2,1),
	zlim = range(seq(0,0.6,0.1))
)

fig5










