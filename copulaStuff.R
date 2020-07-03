
myMvd1 <- mvdc(copula = archmCopula(family = "clayton", param = 2),
	margins = c("norm", "norm"), paramMargins = list(list(mean = 0,
	sd = 1), list(mean = 0, sd = 1)))
myMvd2 <- mvdc(copula = archmCopula(family = "frank", param = 5.736),
	margins = c("norm", "norm"), paramMargins = list(list(mean = 0,
	sd = 1), list(mean = 0, sd = 1)))
myMvd3 <- mvdc(copula = archmCopula(family = "gumbel", param = 2),
	margins = c("norm", "norm"), paramMargins = list(list(mean = 0,
	sd = 1), list(mean = 0, sd = 1)))
	par(mfrow = c(1, 3), mar = c(2, 2, 1, 1), oma = c(1, 1, 0, 0),
	mgp = c(2, 1, 0))

persp(myMvd1, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3))
contour(myMvd2, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3))
contour(myMvd3, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3))


myMvd1 <- mvdc(copula = archmCopula(family = "gaussian", param = 2),
	margins = c("norm", "norm"), paramMargins = list(list(mean = 0,
	sd = 1), list(mean = 0, sd = 1)))
persp(myMvd1, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3))

persp(normalCopula(-0.8), dcopula)
persp(claytonCopula(2), pcopula)


normalCopula(0.5)

norm.cop <- normalCopula(0.5)
norm.cop
x <- rcopula(norm.cop, 100)
#plot(x)
dcopula(norm.cop, x)
pcopula(norm.cop, x)
persp(norm.cop, dcopula)
contour(norm.cop, pcopula)
## a 3-dimensional normal copula
u <- rcopula(normalCopula(0.5, dim = 3), 1000)
scatterplot(u)
## a 3-dimensional clayton copula
v <- rcopula(claytonCopula(2, dim = 3), 1000)
## scatterplot3d(v)
x

x <- mvdc(normalCopula(0.75), c("norm", "exp"),
list(list(mean = 0, sd =2), list(rate = 2)))
x.samp <- rmvdc(x, 100)
dmvdc(x, x.samp)
pmvdc(x, x.samp)
persp(x, dmvdc, xlim = c(-4, 4), ylim=c(0, 1))









