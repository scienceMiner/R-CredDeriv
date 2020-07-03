
# Author: Markus Gesmann, 14/11/2006
# Copulae examples in R
# First example
# Show how to use the copula package and how to plot a copula
require(copula)
# Create a gaussian copula of dim=2 with correlation rho=0.71
rho=0.7
myCop=normalCopula(param=rho, dispstr="un")
# We can sample now from the copula, via
rcopula(myCop,10)
# We specify a multivariate distribution based on our copula,
# with marginal distrbutions and parameters
myMvdc=mvdc(myCop, margins=c("norm", "norm"), paramMargins=list(list(mean=1, sd=2), list(mean=0,sd=0)))

# To plot some nice graphs of our copula, we create a grid of 25x25 points in the [0,1]^2 area
n=25
ind <- expand.grid(xi=1:n, xj=1:n)
# Get the denisty of the copula for all the grid points
zd <- dcopula(myCop, c(x=ind$xi/n, y=ind$xj/n))
Md <- data.frame(ind, z=zd)
# transform the data into a matrix for plotting purpose
zd <- as.matrix( reshape(Md, timevar="xi", idvar="xj", direction="wide")[,-1] )

# Create a little function to plot our copula
# based on kde2dplot by Romain Francois
my3dplot <- function(z, zlab="copula",               # a matrix
                      ncol=20,          # the number of colors to use
                      zlim=c(0,max(z)), # limits in z coordinates
                      nlevels=20,       # see option nlevels in contour
		      theta=20,         # see option theta in persp
		      phi=30           # see option phi in persp
		      )
		      {
nrz <- nrow(z)
ncz <- ncol(z)
couleurs  <- tail(topo.colors(trunc(1.4 * ncol)),ncol)
fcol      <- couleurs[trunc(z/zlim[2]*(ncol-1))+1]
dim(fcol) <- c(nrz,ncz)
fcol      <- fcol[-nrz,-ncz]
persp(z,zlim=zlim,col=fcol,theta=theta,phi=phi,zlab=zlab, xlab="x",main=main)
#image(z,col=couleurs)
#contour(z,add=T,nlevels=nlevels)

#invisible(fcol)
}

#op <- par(mfrow=c(2,2))
my3dplot(zd)
#title(bquote(rho == .(rho)))

# Get the propabilities as well:
zp <- pcopula(myCop, c(x=ind$xi/n, y=ind$xj/n))
M <- data.frame(ind, z=zp)
# transform the data into a matrix for plotting purpose
zp <- as.matrix( reshape(M, timevar="xi", idvar="xj", direction="wide")[,-1] )
my3dplot(zp,main="Cumulative Distribution")
par(op)
title(bquote(rho == .(rho)))

# Fancy 3d plots
require(rgl)
ncol <- 20
col.ind <- as.numeric(cut(zp,ncol))
cols <- tail(topo.colors(trunc(1.4 * ncol)),ncol)[col.ind]
Mcols <- as.matrix( reshape(data.frame(ind, cols), timevar="xi", idvar="xj",
 	 direction="wide")[,-1] )
 	 
# PDF
clear3d()
surface3d(1:n, 1:n, zd*10, col=Mcols,back="lines")

# or this one: CDF
clear3d()
surface3d(1:n, 1:n, zp*10, col=Mcols,back="lines")


## Second example
## Assume US interest rates are log-normal with mean 0.08 and sd 0.02,
## GBP rate to USD has mean of 1.70 with sd 0.1 and is normal,
## DM rate to USD has mean 0.63 with sd 0.08  and is normal.

## How much money will I have after one year, when 100,000 are invested
## in USD with interest rates -- and 50,000 GBP and 200,000 DM with exchange
## rates as above.

## Let's start with multivariate normal distribution
library(MASS)
# Define the correlation matrix
Cor=matrix(c(1,-0.7,-.5,-0.7,1,0.6,-0.5,0.6,1), nrow=3)
# Create random numbers with mean=0 and correlation
x=mvrnorm(10000,mu=rep(0,3), Cor)  ## x \in (-oo, oo)^3
summary(x)
CDF=pnorm(x)  ## CDF  \in [0,1]^3
summary(CDF)

## mean and variance for lognormal
mu <- function(ex,vx){ log(ex^2/sqrt(vx^2+ex^2))}
sig <- function(ex,vx){ sqrt(log((vx/ex)^2+1)) }

InRate <- qlnorm(CDF[,1], meanlog=mu(0.08,0.02), sdlog=sig(0.08, 0.02) )
GBP <- qnorm(CDF[,2], mean=1.70, sd=0.1)
DM <- qnorm(CDF[,3], mean=0.63,sd=0.08)

summary(100000*(1+InRate)+50000*GBP+200000*DM)
plot(density(100000*(1+InRate)+50000*GBP+200000*DM))


## Do the same now with a Gaussian copula

library(copula)
# Define a normal copula with the above correlation matrix
myCop=normalCopula(param=c(-0.7,-.5,0.6), dim = 3, dispstr = "un")
## Create a multivariate distribution with our defined copula
myMvd <- mvdc(copula=myCop, margins=c("lnorm", "norm", "norm"),
            paramMargins=list(list(meanlog=mu(0.08,0.02), sdlog=sig(0.08, 0.02)),
                list(mean=1.70, sd=0.1), list(mean=0.63,sd=0.08)) )

## Sample from the mvdc
InRate.GBP.DM <- rmvdc(myMvd, 10000)
## Compare with our previous approach

summary(100000*(1+InRate.GBP.DM[,1])+50000*InRate.GBP.DM[,2]+200000*InRate.GBP.DM[,3])

