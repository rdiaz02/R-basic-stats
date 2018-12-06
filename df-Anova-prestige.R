## Different degrees of freedom from car::Anova and stats::anova (and
## strange Sums of Squares from car::Anova?)


## Strange degrees of freedom from car::Anova with type II SS.



## See further comments here: https://stat.ethz.ch/pipermail/r-help/2018-December/460705.html
## and John Fox's answer: https://stat.ethz.ch/pipermail/r-help/2018-December/460705.html


    




library(car)
data(Prestige)

prestige_nona <- na.omit(Prestige)

######### As expected
Anova(lm(prestige ~ type * income * women, data = prestige_nona))
m_1 <- lm(prestige ~ type * income, data = prestige_nona)
m_2 <- lm(prestige ~ type * income + women, data = prestige_nona)
sum(residuals(m_1)^2) - sum(residuals(m_2)^2)


######### As expected
Anova(lm(prestige ~ education * income * women, data = prestige_nona))
m_1 <- lm(prestige ~ education * income, data = prestige_nona)
m_2 <- lm(prestige ~ education * income + women, data = prestige_nona)
sum(residuals(m_1)^2) - sum(residuals(m_2)^2)

######### As expected
Anova(lm(prestige ~ education * type * women, data = prestige_nona))
m_1 <- lm(prestige ~ education * type, data = prestige_nona)
m_2 <- lm(prestige ~ education * type + women, data = prestige_nona)
sum(residuals(m_1)^2) - sum(residuals(m_2)^2)


######### As expected
Anova(lm(prestige ~ education * type * income, data = prestige_nona))
m_1 <- lm(prestige ~ education * type, data = prestige_nona)
m_2 <- lm(prestige ~ education * type + income, data = prestige_nona)
sum(residuals(m_1)^2) - sum(residuals(m_2)^2)


######### As expected
Anova(lm(prestige ~ education * type * income, data = prestige_nona))
m_1 <- lm(prestige ~ education * type, data = prestige_nona)
m_2 <- lm(prestige ~ education * type + income, data = prestige_nona)
sum(residuals(m_1)^2) - sum(residuals(m_2)^2)



######### Why??
## strange df
Anova(lm(prestige ~ women * type * income * education, data = prestige_nona))
## but notice
Anova(lm(prestige ~ women * type * income * education, data = prestige_nona),
      type = "III")
anova(lm(prestige ~ type * income * education * women, data = prestige_nona))
m_1 <- lm(prestige ~ type * income * education, data = prestige_nona)
m_2 <- lm(prestige ~ type * income * education + women, data = prestige_nona)
## does not match women SS
sum(residuals(m_1)^2) - sum(residuals(m_2)^2) 

## Looking at the code, I do not understand what the call from
## linearHypothesis returns here (specially compared to other models), and
## the problem seems to be in the return from ConjComp, due to the the
## vcov of the model?




######### As expected
Anova(lm(prestige ~ education * census * women, data = prestige_nona))
m_1 <- lm(prestige ~ education * census, data = prestige_nona)
m_2 <- lm(prestige ~ education * census + women, data = prestige_nona)
sum(residuals(m_1)^2) - sum(residuals(m_2)^2)


######### Why??
Anova(lm(prestige ~ census * type * income * women, data = prestige_nona))

Anova(lm(prestige ~ census * type * income * women, data = prestige_nona),
         type = "III")

anova(lm(prestige ~ census * type * income * women, data = prestige_nona))
m_1 <- lm(prestige ~ census * type * income, data = prestige_nona)
m_2 <- lm(prestige ~ census * type * income + women, data = prestige_nona)
sum(residuals(m_1)^2) - sum(residuals(m_2)^2)


######### Why??
Anova(lm(prestige ~ census * education * income * women, data = prestige_nona))
m_1 <- lm(prestige ~ census * education * income, data = prestige_nona)
m_2 <- lm(prestige ~ census * education * income + women, data = prestige_nona)
sum(residuals(m_1)^2) - sum(residuals(m_2)^2)

######### Why??
Anova(lm(prestige ~ census * education * type * women, data = prestige_nona))
m_1 <- lm(prestige ~ census * education * type, data = prestige_nona)
m_2 <- lm(prestige ~ census * education * type + women, data = prestige_nona)
sum(residuals(m_1)^2) - sum(residuals(m_2)^2)







