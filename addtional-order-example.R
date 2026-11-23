## An additional example of changes with order
## and equivalence of testing procedures

library(car)
load("additiona-order-example-data.RData")
## dms2$y2 <- dms2$y + rnorm(nrow(dms2), sd = 4)


xtabs( ~ Var1 + Var2, data = dms2)

xtabs( ~ Var1 + Var2 + Var3, data = dms2)

xtabs(y2 ~ Var1 + Var2 + Var3,
      data = aggregate(y2 ~ Var1 + Var2 + Var3,
                       FUN = mean,
                       data = dms2))

xtabs(y2 ~ Var1 + Var2,
      data = aggregate(y2 ~ Var1 + Var2,
                       FUN = mean,
                       data = dms2))

xtabs(y2 ~ Var3,
      data = aggregate(y2 ~ Var3,
                       FUN = mean,
                       data = dms2))


m3 <- lm(y2 ~ Var1 * Var2 + Var3, data = dms2)
m3b <- lm(y2 ~ Var3 + Var1 * Var2, data = dms2)
m4 <- lm(y2 ~ Var1 * Var2, data = dms2)
m5 <- lm(y2 ~ Var3 + Var1 + Var2, data = dms2)

anova(m3b)
anova(m3)
Anova(m3)
## Identical. Testing Var3
anova(m3b, m4)
anova(m4, m3b)

## Testing interaction Var1:Var2
anova(m5, m3)
anova(m5, m3b)
