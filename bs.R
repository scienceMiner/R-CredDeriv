
bsf <- function(S,t,E,r,sigma,T)
{
tau <- T-t;
if (tau > 0)
{
	d1 <- (log(S/E) + (r + 0.5*sigma^2)*tau)/(sigma*sqrt(tau))
	d2 <- d1 - sigma*sqrt(tau)
	C <- S*pnorm(d1)-E*exp(-r*tau)*pnorm(d2)
}
else
	return(max(S-E,0))
}

s <- seq(0.05,2.5,by=0.05)
t <- seq(0.02,1.0,by=0.02)

t1 <- rep(t,50)
s1 <- rep(s, each=50)
t1
s1

m1 <- cbind(t1,s1)

apply(m1,1, bsf, S="t1", t="s1", E = 1, r = 0.05, sigma = 0.25, T = 1)


cave <- function(cJ,c1,c2) 
{
	return (s+t+c1+c2)
}


x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
x1
x2
x11 <- cbind(x1 = c(3:9))
x11

##- function with extra args:
apply(x11,2, cave,  s="x1", t=3, c1=5,c2=15)



mapply(rep, 1:4, 4:2)

mapply(rep, times=1:4, x=4:1)

mapply(rep, times=1:4, MoreArgs=list(x=42))

## Compute row and column sums for a matrix:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
apply(x, 2, mean, trim = .2)
col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, sum)
rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))

stopifnot( apply(x, 2, is.vector))

## Sort the columns of a matrix
#apply(x, 2, sort)
x
##- function with extra args:
cave <- function(x, c1 , r , c2) c(x[c1]+r+2*x[c2])

apply(x,1, cave,  c1="x1", c2="x2",r = 0)


bsf2 <- function(matri,c1,c2,E,r,sigma,T)
{
S=matri[c1];
t=matri[c2];
tau <- T-t;
if (tau > 0)
{
	d1 <- (log(S/E) + (r + 0.5*sigma^2)*tau)/(sigma*sqrt(tau))
	d2 <- d1 - sigma*sqrt(tau)
	C <- S*pnorm(d1)-E*exp(-r*tau)*pnorm(d2)
	c(C)
}
else
	return(c(max(S-E,0)))
}

s <- seq(0.05,2.5,by=0.05)
t <- seq(0.02,1.0,by=0.02)

t1 <- rep(t,50)
s1 <- rep(s, each=50)

m1 <- cbind(t1,s1)

bs <- apply(m1,1, bsf2, c1="s1", c2="t1", E = 1, r = 0.05, sigma = 0.25, T = 3)


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


