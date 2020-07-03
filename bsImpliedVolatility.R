

bs.Call <- function(S,E,r,sigma,T)
{
	d1 <- (log(S/E) + (r + 0.5*sigma^2)*T)/(sigma*sqrt(T))
	d2 <- d1 - sigma*sqrt(T)
	premium <- S*pnorm(d1)-E*exp(-r*T)*pnorm(d2)
	list("premium"=premium)
}

#T=1
#r=0.03
f <- function(sigma,param)
{
	bs.Call(param$S,param$E,param$rate,sigma,param$T)$premium - param$premium2

}

param <- data.frame(rate=0,S=0,E=0,Time=0,premium2=0.16)

param$rate <- 0.03
param$S <- 10
param$E <- 11
param$Time <-  1
param$Premium2 <- 0.16

iv<-uniroot(f,interval=c(0,1),param)
iv$root



    bs.res <- function(vol,row) {
        ret <- NA
        if(row$Type == "call")
            ret <- bs.call(vol,strike.price=row$Strike,Stock.price=row$Stock.price,TTM=row$TTM/365,rate=row$Short.rate/100) - row$Last
            }

    find.root <- function(row) {
        if(bs.res(0,row)*bs.res(upper,row) >= 0)
             vol <- NA
        else 
            vol <- uniroot(bs.res, interval = c(0,upper),row=row)$root
            
        return(vol)
    }

