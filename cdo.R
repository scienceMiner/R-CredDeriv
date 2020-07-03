
Simulations <- 10000
Loans <- 50
Tranches <- 3

w <- 0.3 ## what is this in the Nomura correlation primer doc?

attachmentPoints <- c(0.0,0.03,0.07,1)
EADs <- rep(1,Loans)
LGDs <- rep(0.5,Loans)
defaultProbs <- rep(0.01,Loans)
attachmentPoints
EADs
LGDs
defaultProbs

sumEADs <- sum(EADs)
sumEADs

normalizedEADs <- EADs/sumEADs
normalizedEADs

d <- qnorm(defaultProbs)
d

wArray <- rep(w,Loans)
w2Array <- rep( (1 - w*w)^0.5, Loans)
wArray
w2Array

# rnorm(1,mean=0,sd=1) 
factor <- rnorm(1)
factor

tranchePD <- rep(0,length(attachmentPoints)-1)
currentLoss <- rep(0,Loans)
length(attachmentPoints)

for(i in 1:Simulations) 
{
	factor <- rnorm(1)
	factor
	loss_j <- 0
	for (j in 1:Loans)
	{
		if ((wArray[j] * factor + w2Array[j] * rnorm(1)) < d[j]) 
		loss_j <- (loss_j + (LGDs[j] * normalizedEADs[j]))
	}
  
	for (k in 1:(length(attachmentPoints)-1))
	{
		if (loss_j - attachmentPoints[k] > 10^-15)
		{
			tranchePD[k] = tranchePD[k] + 1/Simulations
		}
	}

}

loss_j

tranchePD


simCDO <- function(matri,c1,c2,Sims,PortfolioSize)
{

lgd=matri[c1];
w=matri[c2];

Simulations <- Sims
Loans <- PortfolioSize
Tranches <- 3
attachmentPoints <- c(0.0,0.03,0.07,1)
EADs <- rep(1,Loans)
LGDs <- rep(lgd,Loans)
defaultProbs <- rep(0.01,Loans)

sumEADs <- sum(EADs)
normalizedEADs <- EADs/sumEADs
d <- qnorm(defaultProbs)
wArray <- rep(w,Loans)
w2Array <- rep( (1 - w*w)^0.5, Loans)
factor <- rnorm(1)

tranchePD <- rep(0,length(attachmentPoints)-1)

for(i in 1:Simulations) 
{
	factor <- rnorm(1)
	factor
	loss_j <- 0
	for (j in 1:Loans)
	{
		if ((wArray[j] * factor + w2Array[j] * rnorm(1)) < d[j]) 
		loss_j <- (loss_j + (LGDs[j] * normalizedEADs[j]))
	}
  
	for (k in 1:(length(attachmentPoints)-1))
	{
		if (loss_j - attachmentPoints[k] > 10^-15)
		{
			tranchePD[k] = tranchePD[k] + 1/Simulations
		}
	}

}

	return(c(tranchePD))

}



factor <- seq(0.1,1.0,by=0.1)
tlgd <- seq(0.1,1.0,by=0.1)

t1 <- rep(factor,50)
s1 <- rep(tlgd, each=50)

m1 <- cbind(t1,s1)

CDOvals <- apply(m1,1, simCDO, c1="s1", c2="t1",1000,10)

CDOvals

trellis.par.set("axis.line", list(col="transparent"),
	clip = list(panel = "off"))

trellis.par.set(theme = col.whitebg())
poly.border<-trellis.par.get("box.rectangle")
poly.border$border<-"green"
trellis.par.set("box.rectangle",poly.border)

fig5 <- wireframe(bs ~ t1 * s1, #g,
	scales = list(arrows=FALSE, cex= .55, col = "black", font = 3), 
	drape = TRUE,
	screen = list(z = -50, x = -75),
	aspect = c(1,1),
	light.source = c(100,0,50),
	colorkey = FALSE,
	xlab = expression(paste(Time)),
	ylab = expression(paste(Stock)),
	zlab = list(label = "Call"),
	split = c(1,1,2,1),
	zlim = range(seq(0.00,1.70,by=0.01))
)

fig5




