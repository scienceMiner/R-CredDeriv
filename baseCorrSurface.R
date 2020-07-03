

dates<-c("20 Sept 2007","22 Oct 2007","22 Nov 2007")
m1<-matrix(rep(0,25),5,5)
m1[1,]<-c(0.1320,0.0139515,0.0047955,0.0027205,0.001565) # 20 Sept 2007
m1[2,]<-c(0.1320,0.0245455,0.0093075,0.003949,0.0020975) # 22 Oct 2007
m1[3,]<-c(0.1350,0.027108,0.0113045,0.006449,0.004225) # 22 Nov 2007
m1[4,]<-c(0.1101,0.0234,0.00855,0.0036,0.00085) # 22 Nov 2007
m1[5,]<-c(0.1396,0.0160,0.0008045,0.0000349,0.0000025) # 22 Nov 2007




TrancheCorr<- rep(0,length(m1))

	TrancheCorr[1] <- implied_corr2(m1[1,1],0.00,0.03,initialCorr=0.2)

	TrancheCorr[2] <- base_correlation(m1[1,2],TrancheCorr[1],0.06,initialCorr=0.45)

	TrancheCorr[3] <- base_correlation(m1[1,3],TrancheCorr[2],0.09,initialCorr=0.5)

	TrancheCorr[4] <- base_correlation(m1[1,4],TrancheCorr[3],0.12,initialCorr=0.8)

	TrancheCorr[5] <- base_correlation(m1[1,4],TrancheCorr[4],0.22,initialCorr=0.8)




# baseCorrlationFunction
base_correlation_all <- function(m1)
{
	TrancheCorr<- rep(0,length(m1))

	TrancheCorr[1] <- implied_corr2(m1[1],0.00,0.03,initialCorr=0.7)

	TrancheCorr[2] <- base_correlation(m1[2],TrancheCorr[1],0.06,initialCorr=0.45)

	TrancheCorr[3] <- base_correlation(m1[3],TrancheCorr[2],0.09,initialCorr=0.5)

	TrancheCorr[4] <- base_correlation(m1[4],TrancheCorr[3],0.12,initialCorr=0.8)

	TrancheCorr[5] <- base_correlation(m1[5],TrancheCorr[4],0.22,initialCorr=0.8)

	return(TrancheCorr)
}

TrancheCorr<-matrix(rep(0,25),5,5)
TrancheCorr[1,] <- base_correlation_all(m1[1,])
TrancheCorr[2,] <- base_correlation_all(m1[2,])
TrancheCorr[3,] <- base_correlation_all(m1[3,])
TrancheCorr[4,] <- base_correlation_all(m1[4,])
TrancheCorr[5,] <- base_correlation_all(m1[5,])



# Now we have a trancheCorr Array - apply to a particular date

x<-c(2,3,4,4,5,6)
y<-c(3,6,7,8,9,12)
test<-function(x)
{
	testRes<-rep(0,length(x))
	testRes[1]<-x[1]
	testRes[2]<-x[2]*x[2]
	testRes[3]<-x[3]*x[3]
	testRes[4]<-x[4]*x[4]
	testRes[5]<-x[5]*x[5]
	testRes[6]<-x[6]*x[6]
	return(testRes)
}

tr <- matrix(rep(0,12),2,6)
tr[1,] <- test(x)
tr[2,] <- test(y)

tr[2,4]


m.m<-4
names<-c("DE","GB","FR","IT")
w<-matrix(c(0.045, 0.096, 0.035, 0.016))
b<-matrix(c(1.02,0.97,1.11,1.05))
m.0<-b*m.m
dimnames(m.0)<-list(names,"prior")
data<-cbind(w,b,m.0)
s.m<-sqrt(4/(0.0151))
cov<-beta%*%t(beta)*26^2+diag(rep(5^2,18))
dimnames(data)<-list(names,c("w","beta","m.0"))
P<-matrix(c(1,-1,rep(0,16)),1,18)
dimnames(P)<-list("fcst",names)










