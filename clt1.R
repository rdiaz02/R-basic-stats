



library(parallel)

clt1 <- function(n_pop, n_mean,  FUN, ...) {
    cores <- detectCores()
    means <- unlist(mclapply(seq.int(n_mean),
                             function(u) mean(FUN(n_pop, ...)),
                             mc.cores = cores))
    op <- par(mfrow = c(2, 1))
    hist(means, breaks = 50)
    abline(v = mean(means))
    qqnorm(means, pch = ".")
    qqline(means)
}


clt1(2, 10000, rexp, 0.1)

clt1(10, 10000, rexp, 0.1)

clt1(20, 10000, rexp, 0.1)

clt1(50, 10000, rexp, 0.1)

clt1(100, 10000, rexp, 0.1)

clt1(200, 10000, rexp, 0.1)

## cauchy always behaves poorly
clt1(5, 10000, rcauchy, location = 5)
clt1(20, 10000, rcauchy, location = 5)
clt1(100, 10000, rcauchy, location = 5)
clt1(1000, 10000, rcauchy, location = 5)
clt1(2000, 10000, rcauchy, location = 5)


## What about a t distribution?
library(limma)

tdt1 <- function(n_pop, n_mean,
                 truemean = NULL,
                 FUN, ...) {
    cores <- detectCores()

    X <- mclapply(seq.int(n_mean),
                  function(u) FUN(n_pop, ...),
                  mc.cores = cores)

    Xmat <- do.call(rbind, X)
    ## or rbindlist (data.table) or dplyr::bind_rows, etc
    means <- rowMeans(Xmat)

    if(is.null(truemean)) 
        truemean <- mean(means)
    vars <- apply(Xmat, 1, var)
    se <- sqrt(vars/n_pop)
    tstats <- (means - truemean)/se

    op <- par(mfrow = c(2, 1))
    hist(tstats, freq = FALSE)
    curve(dt(x, n_pop -1, 0), -4, 4, add = TRUE)
    qqt(tstats, df = n_pop - 1, pch = ".")
    abline(0, 1)
}

tdt1(2, 10000, 10, rexp, 0.1)

tdt1(20, 10000, 10, rexp, 0.1)

tdt1(50, 10000, 10, rexp, 0.1)

tdt1(100, 10000, 10, rexp, 0.1)

tdt1(200, 10000, 10, rexp, 0.1)

tdt1(500, 10000, 10, rexp, 0.1)

tdt1(5000, 10000, 10, rexp, 0.1)


tdt1(1000, 10000, 5, rcauchy, 5)

tdt1(5000, 10000, 5, rcauchy, 5)



## we could plot the density
   ## The next is an ugly kludge, since we can plot the
    ## dwhaterver from rwhatever
    ## But would require work to find domain, etc
    ## dfun <- gsub("^r", "d", deparse(substitute(FUN)))
    ## eval the dfun, then plot using curve

    ## But this is sooooo ugly
    ## npd <- 1e6
    ## hist(FUN(npd, ...), freq = FALSE, xlab = "Original distr.",
    ##      breaks = 500)

    ## dfun <- gsub("^r", "d", deparse(substitute(FUN)))
## eval the dfun, then plot using curve

## get range from the min and max of the random numbers



## this will not work
clt2 <- function(n_mean, n_pop, FUN, ...) {
    means <- replicate(n_mean, mean(FUN(n_pop, ...)))
    op <- par(mfrow = c(1, 2))
    hist(means)
    abline(v = mean(means))
    qqnorm(means)
}

clt2(100, 10, rexp, lambda = 0.1)
## see help of replicate:
## If ‘expr’ is a function call, be aware of assumptions about where
## it is evaluated, and in particular what ‘...’ might refer to.  You
## can pass additional named arguments to a function call as
## additional named arguments to ‘replicate’: see ‘Examples’.


## But this does
clt3 <- function(n_mean, n_pop,  FUN, ...) {
    f1 <- function() FUN(n_pop, ...)
    means <- replicate(n_mean, mean(f1()))
    op <- par(mfrow = c(1, 2))
    hist(means)
    abline(v = mean(means))
    qqnorm(means)
}

clt3(100, 10, FUN = rexp, 0.1)
