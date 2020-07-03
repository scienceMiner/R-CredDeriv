
date1<-matrix(c('Jan07','Mar07','31Mar07','30Apr07','30May07','29Jun07','31Jul07','28Sep07','30Nov07','31Jan08','31Mar08','30May08','31Jul08'))
eq<-matrix(c(10.22,9.80,11.04,9.76,6.24,11.63,29.75,18.79,25.33,30.25,38.90,33.53,31.01))
mezz<-matrix(c(41.18,45.26,57.18,49.03,39.34,61.42,217.66,89.5,158.92,309.80,467.90,293.92,351.10))
midsen<-matrix(c(11.5,12.07,13.64,11.95,9.83,15.71,84.5,34.42,82.83,205.6,296.20,184.09,215.00))
sen<-matrix(c(5.18,5.09,5.46,5.02,3.98,6.99,43.4,21.91,59.75,133.00,206.1,122.21,136.10))
supsen<-matrix(c(1.61,1.74,2.16,2.07,1.54,2.61,22.2,13.25,34.00,69.20,100.1,57.13,66.40))

z<-ts(matrix(c(mezz,midsen,sen),13,3), start=c(2007,1), frequency = 7)

# fit a line to the points
#myline.fit <- lm(y ~ x)

# get information about the fit
#summary(myline.fit)

# draw the fit line on the plot
#abline(myline.fit) 

plot(z, plot.type="single",type="l", ylab="CDO Tranche Values (bps)", main="iTraxx Tranche Prices 2007-2008",lty=1:3:5, col=4:2:5 )
#par("usr")
legend(2007, 475, c("Equity Tranche", "Mezzanine Tranche", "Senior Tranche"), col = 4:2:5, lty=1:3:5) 
