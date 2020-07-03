
manyroots <- function(f, interval, ints=1, maxlen=NULL,
  lower = min(interval), upper = max(interval),
  tol = .Machine$double.eps^0.25, maxiter = 1000, ...)
{
    if (!is.numeric(lower) || !is.numeric(upper) || lower >=
        upper)
        stop("lower < upper is not fulfilled")
    if (is.infinite(lower) || is.infinite(upper))
        stop("Interval must have finite length")
    if (!is.null(maxlen))
        ints <- ceiling((upper-lower)/maxlen)
    if (!is.numeric(ints) || length(ints)>1 || floor(ints)!=ints || ints<1)
        stop("ints must be positive integer")

    ends <- seq(lower, upper, length=ints+1)
    fends <- numeric(length(ends))
    for (i in seq(along=ends)) fends[i] <- f(ends[i], ...)

    zeros <- iters <- prec <- rep(NA, ints)

    for (i in seq(ints)) {
        cat(i, ends[i], ends[i+1], fends[i], fends[i+1], "\n")
        if (fends[i] * fends[i+1] > 0) {
# cat("f() values at end points not of opposite sign\n")
            next;
        }
        if (fends[i] == 0 & i>1) {
# cat("this was found in previous iteration\n")
            next;
        }
        
        val <- .Internal(zeroin(function(arg) f(arg, ...), ends[i],
            ends[i+1], tol, as.integer(maxiter)))
        if (as.integer(val[2]) == maxiter) {
            warning("Iteration limit (", maxiter, ") reached in interval (",
              ends[i], ",", ends[i+1], ").")
        }
        zeros[i] <- val[1]
        iters[i] <- val[2]
        prec[i] <- val[3]
    }
    zeros <- as.vector(na.omit(zeros))
    fzeros <- numeric(length(zeros))
    for (i in seq(along=zeros)) fzeros[i] <- f(zeros[i], ...)

    list(root = zeros, f.root = fzeros,
        iter = as.vector(na.omit(iters)),
        estim.prec = as.vector(na.omit(prec)))
}

gg <- function(x) x*(x-1)*(x+1)
manyroots(gg, c(-4,4), 13, maxiter=200, tol=10^-10)

hh <- function(x,x2) x^2-x2
manyroots(hh, c(-10, 10), maxlen=.178, x2=9)

manyroots(sin, c(-4,20), maxlen=.01)
#but
ss <- function(x) sin(x)^2
manyroots(ss, c(-4,20), maxlen=.01)
plot(ss, -4,20)
abline(h=0) 