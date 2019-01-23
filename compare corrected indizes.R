truemodel <- '
V2 ~ V1+V3
V1 ~~ 0*V3
'

commodel <- '
V2 ~ V1+V3
V1 ~~ V3
'

pv <- function(truemodel, commodel, obs, n){
  pvalues = list()
  for (i in 1:n){
    truedat <- simulateData(truemodel, model.type = "sem", sample.nobs = obs)
    truefit <- sem(truemodel, truedat)
    overfit <- sem(commodel, truedat)
    x <- anova(truefit, overfit)
    pvalues[i] <- x[[2,7]]
  }
  return(pvalues)
}

x <- pv(truemodel, commodel, 50, 1000)

data.frame(p = unlist(x)) %>% ggplot(aes( x = p)) + geom_density()



truedat <- simulateData(truemodel, model.type = "sem", sample.nobs = 50)

truefit <- sem(truemodel, truedat)

summary(truefit, fit.measures = TRUE)



overfit <- sem(commodel, truedat)

summary(overfit, fit.measures = TRUE)

anova(truefit, overfit)
