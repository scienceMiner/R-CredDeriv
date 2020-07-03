
a <- c(0.05,0.1,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.5)
bias <- c(0.017,0.045,0.067,0.0997,0.123,0.156,0.195,0.228,0.276,0.329)
phi <- c(0.01,0.01,0.05,0.05,0.05,0.15,0.15,0.15,0.55,0.55)

g <- expand.grid(x = a, y = bias, z = phi)

fig5 <- wireframe(y ~ x * z, g, drape = TRUE, colorkey = TRUE, zlim = range(seq(0.00,0.61,by=0.05))))

fig5

trellis.par.set("axis.line", list(col="transparent"))
trellis.par.set(theme = col.whitebg())
     
a <- c(0.05,0.1,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.5,0.05,0.1,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.5,0.05,0.1,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.5,0.05,0.1,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.5)
bias <- c(0.017,0.045,0.067,0.0997,0.123,0.156,0.195,0.228,0.276,0.329,0.021,0.0498,0.0692,0.1032,0.134,0.1633,0.2014,0.231,0.2965,0.3567,0.0243,0.0538,0.0722,0.1132,0.124,0.1733,0.2114,0.241,0.3065,0.3667,0.0263,0.055,0.076,0.122,0.13,0.183,0.224,0.25,0.315,0.3774)
phi <- c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20)

#g <- expand.grid(x = a, y = phi, z = bias)

fig5 <- wireframe(bias ~ a * phi, #g,
	scales = list(arrows=FALSE, cex= .45, col = "black", font = 3), 
	drape = TRUE,
	screen = list(z = -220, x = -75),
	aspect = c(3,1), 
	colorkey = FALSE,
	xlab = expression(paste(alpha)),
	ylab = expression(paste(phi)),
	zlab = list(label = "Bias", font = 1, cex = 0.60),
	ylim = range(seq(0.05,0.20,by=0.05)),
	zlim = range(seq(0.00,0.4,by=0.02))
)

fig5



ols <- cbind(a, bias, phi)

fig6 <- wireframe(bias ~ a * phi, 
 data = ols,
 scales = list(arrows=FALSE, cex= .45, col = "black", font = 3), 
 shade = TRUE, colorkey = FALSE,
 screen = list(z = -245, x = -75),
 xlab = expression(paste(alpha)),
 ylab = expression(paste(phi)), 
 zlab = list(label = "Bias", font = 1, cex = 0.60), 
 zlim = range(seq(0.0, 0.85, by=0.20)))



require(lattice)

x <- seq(-pi, pi, len = 20)
y <- seq(-pi, pi, len = 20)
g <- expand.grid(x = x, y = y)
g$z <- sin(sqrt(g$x^2 + g$y^2))
print(wireframe(z ~ x * y, g, drape = TRUE,aspect = c(3,1), colorkey = TRUE))


