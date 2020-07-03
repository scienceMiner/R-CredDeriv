require(fCalendar)
require(fImport)

# from 4th April 2007 to 6th Oct 2008
query = "s=^GSPC&a=08&b=3&c=2007&d=10&e=6&f=2008&x=.csv"
SPX = yahooImport(query)
SPX@data

query = "s=RBS.L&a=08&b=3&c=2007&d=10&e=6&f=2008&x=.csv"
RBS = yahooImport(query)
RBS@data

#print("Size of data is ")
#print(SPX.size())
#write.csv(SPX@data,file="./SPX.csv")

cls <- rep(NA,8)
cls[3] <- "numeric"

SP <- read.csv("./SPX.csv",sep=",",header=TRUE)
SP

SP2 <- cbind(SP[2],SP[5])
SP2


# plot the timeseries

if (!is.null(SPX)) print(SPX@data)

x <- 1:20
z<-filter(x, rep(1/5,5), sides=1) 
z

movingaverage <- function(window, series)
{
	n <- length(series)
	current <- 1
	maSeq <- rep(0,n-(window-1))
	for (i in 1:(n-(window-1)))
	{
		end<-i+(window-1)
		maSeq[i] = sum(series[i:end])/window;
		current = current+1
	}
	
	return(maSeq);
}


EWmovingaverage <- function(weight, series)
{
	alpha <- 2/(weight + 1)

	n <- length(series)
	EWMA <- rep(0,n)
	EWMA[1] = series[1]
	for (i in 2:(n))
	{
		EWMA[i] = series[i]*alpha + EWMA[i-1]*(1-alpha)
	}
	
	return(EWMA);
}



FASTstochasticOsc <- function(lastNdays, series)
{
	n <- length(series)
	if (lastNdays >= length(series))
	{
		print("Series not long enough");
		return(series);
	}	
	FastSO <- rep(0,n-lastNdays)
	current<-1
	for (i in (lastNdays+1):(n))
	{
		low<-min(series[(i-(lastNdays-1)):i])
		high<-max(series[(i-(lastNdays-1)):i])
		numerator = series[i] - low
		denom = high-low
		FastSO[current]<-numerator/denom * 100
		current<-current + 1
	}	
	return(FastSO);
}

SLOWstochasticOsc <- function(window,lastNdays,series)
{
	return(movingaverage(3,FASTstochasticOsc(lastNdays,series)))
}

plot(SP[5])

MACD<- function(signalSeries,series)
{
	#series1<-EWmovingaverage(1,series)
	series12<-EWmovingaverage(12,series)
	series26<-EWmovingaverage(26,series)
	
	n<-length(series12)
	MACD <- rep(0,n)
	for (i in 1:n)
	{
		MACD[i] <- signalSeries[i] + (series12[i] - series26[i])
	}
	return(MACD)

}

MACD(t(SP[6]))

jj=ts(SP[6], start=c(2007,9), frequency=250)
j2=ts(movingaverage(10,t(SP[6])), start=c(2007,9), frequency=250)
EWj2=ts(EWmovingaverage(10,t(SP[6])), start=c(2007,9), frequency=250)
MACDj2=ts(MACD(EWmovingaverage(9,t(SP[6])),t(SP[6])), start=c(2007,9), frequency=250)
MACD9j2=ts(EWmovingaverage(9,t(SP[6])), start=c(2007,9), frequency=250)
FSOj2=ts(FASTstochasticOsc(14,t(SP[6])), start=c(2007,10), frequency=250)
SSOj2=ts(SLOWstochasticOsc(3,14,t(SP[6])), start=c(2007,10), frequency=250)


MACDj2=
length(FSOj2)
length(SSOj2)

MACDj2=ts(MACD(EWmovingaverage(45,t(SP[6])),t(SP[6])), start=c(2007,9), frequency=250)
MACD9j2=ts(EWmovingaverage(9,t(SP[6])), start=c(2007,9), frequency=250)


plot(jj)
#lines(j2,col="red")
lines(MACDj2,col="blue")
lines(MACD9j2,col="green")

plot(FSOj2,col="green")
lines(SSOj2,col="red")

strategy <- function(amount,EWseries1,EWseries2,origseries)
{
	n<-length(origseries)
	difference<-rep(0,n)
	for (i in 1:n)
	{
		difference[i]<-EWseries1[i]-EWseries2[i]
		#print(difference[i])
		#print(EWseries2[i])
	}
	totalprofit <- 0;shares<-0;
	for (i in 2:n)
	{
	if ((difference[i-1] * difference[i]) < 0)
	{
		if (difference[i-1] < 0 && difference[i] > 0)
		{
			print("BUY");
			shares = amount/origseries[i];
			lastamount = amount;
			#print(amount);
		}
		else
		if (difference[i-1] > 0 && difference[i] < 0)
		{
			print("SELL");
			if (shares != 0)
			{
			amount = shares * origseries[i];
			profit = amount - lastamount
			totalprofit = profit + totalprofit 
			#print(profit)
			}
		}
	}
	}
	print("Total profit is")
	print(totalprofit)
}

strategy(100000,EWmovingaverage(12,t(SP[6])),EWmovingaverage(26,t(SP[6])),t(SP[6]))


strategy(100000,EWmovingaverage(14,t(SP[6])),EWmovingaverage(12,t(SP[6])),t(SP[6]))


strategy(100000,EWmovingaverage(40,t(SP[6])),EWmovingaverage(12,t(SP[6])),t(SP[6]))


strategy(100000,EWmovingaverage(12,t(SP[6])),EWmovingaverage(20,t(SP[6])),t(SP[6]))







time(jj)







