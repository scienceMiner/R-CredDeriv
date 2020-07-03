getBasketSpreadMatrix <- function(matri,c1,c2)
{
	intensity=matri[c2];
	correl=matri[c1];
	return(getBasketSpread(intensity,10,1,correl,0.04,5))

}


intensity <- seq(0.01,0.1,by=0.01)
correlation <- seq(0.04,0.4,by=0.04)

t1 <- rep(intensity,10)
s1 <- rep(correlation,each=10)

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
	screen = list(z = 55, x = -75),
	aspect = c(1,1),
	light.source = c(100,0,50),
	colorkey = FALSE,
	xlab = expression(paste(Intensity)),
	ylab = expression(paste(Correlation)),
	zlab = list(label = "Breakeven Spread", rot = 90),
	split = c(1,1,2,1),
	zlim = range(seq(0,9500,0.1))
)

fig5


