## Contrary to Conover, not really testing F(x)= G(x)
## Wikipedia entry much clearer: is a nonparametric test of the null
## hypothesis that it is equally likely that a randomly selected value
## from one sample will be less than or greater than a randomly selected
## value from a second sample.


## Will not reject
for(i in 1:5) {
    x <- rnorm(50000, mean = 0, sd = 1)
    y <- rnorm(50000, mean = 0, sd = 20)
    print(wilcox.test(x, y))
}


## Not about medians or means either

## Will reject, medians the same
x <- c(rep(10, 1000), 11, rep(12, 1000))
y <- c(rep(10, 1000), 11, rep(13, 1000))
print(summary(x))
print(summary(y))
print(wilcox.test(x, y))


## Will reject, means the same
x <- seq(from = 10, to = 20, length.out = 1000)
y <- c(rep(0, 999), 15000)
print(summary(x))
print(summary(y))
print(wilcox.test(x, y))







## Will not reject, medians differ
x <- c(rep(10, 1000), 11, rep(12, 1000))
y <- c(rep(10, 1000), 10.1, rep(12, 1000))
summary(x)
summary(y)
wilcox.test(x, y)


## Will not reject, means differ
x <- c(rep(10, 1000), 1e9, rep(1000, 1000))
y <- c(rep(10, 1000), -1e9, rep(1000, 1000))
summary(x)
summary(y)
wilcox.test(x, y)





### Paired and transformations

## taking log does not change things when using the two-sample Wilcoxon
wilcox.test(p53 ~ cond, data = dp53)
wilcox.test(log(p53) ~ cond, data = dp53)


###### paired
## Without logs
wilcox.test(dmyc$myc[1:12], dmyc$myc[13:24], paired = TRUE)
## After taking logs
wilcox.test(log(dmyc$myc[1:12]), log(dmyc$myc[13:24]), paired = TRUE)

