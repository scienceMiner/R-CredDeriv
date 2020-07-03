
factor <- seq(0.1,0.9,by=0.05)
tlgd <- seq(0.1,0.9,by=0.05)

t1 <- rep(factor,1)
s1 <- rep(tlgd, 1)

m1 <- cbind(t1,s1)

spreads03 <- apply(m1,1, getSpread2, c1="s1", 0.0, 0.03 )
spreads03

spreads36 <- apply(m1,1, getSpread2, c1="s1", 0.03, 0.06 )
spreads36

spreads69 <- apply(m1,1, getSpread2, c1="s1", 0.06, 0.09 )
spreads69

spreads912 <- apply(m1,1, getSpread2, c1="s1", 0.09, 0.12 )
spreads912

spreads1222 <- apply(m1,1, getSpread2, c1="s1", 0.12, 0.22 )
spreads1222



tranches <- 5

# get the range for the x and y axis
yrange <- range(0,0.06)
y2range <- range(0,0.25)
xrange <- range(0.05,0.9)

# set up the plot
plot(xrange, yrange, axes="F",type="n", xlab="Correlation",ylab="Breakeven Spread" )
axis(2, pretty(range(yrange)))
colors <- rainbow(tranches)
linetype <- c(1:tranches)
plotchar <- seq(18,18+tranches,1)
axis(1,pretty(range(xrange),10))

lines(s1, spreads36, type="b", lwd=1.5,lty=linetype[2], col=colors[2],pch=plotchar[2])
lines(s1, spreads69, type="b", lwd=1.5,lty=linetype[3], col=colors[3],pch=plotchar[3])
lines(s1, spreads912, type="b", lwd=1.5,lty=linetype[4], col=colors[4],pch=plotchar[4])
lines(s1, spreads1222, type="b", lwd=1.5,lty=linetype[5], col=colors[5],pch=plotchar[5])

par(new=T)

plot(xrange, y2range, axes="F",type="n", xlab="Correlation",ylab="Breakeven Spread" )
axis(4, pretty(range(y2range)))
lines(s1, spreads03, type="b", lwd=1.5,lty=linetype[1], col=colors[1],pch=plotchar[1])

# add a title and subtitle
title("CDO Tranche Spreads (Student-t)", "Degrees of Freedom = 12")

# add a legend
leg.txt <- c("Equity (0-3%) (right axis)", "Junior Mezz (3-6%) (left)","Mezz (6-9%) (left)", "Sub Senior (9-12%) (left)", "Senior (12-22%) (left)")
legend(0.55, 0.25, leg.txt, cex=0.8, col=colors,pch=plotchar, lty=linetype, title="Tranche")

# end 

