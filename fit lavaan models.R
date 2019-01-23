pacman::p_load(semTools, matrixcalc, tidyverse, magrittr, clusterGeneration, lavaan, MASS)

matr <- generate_matrices(3, 1, 100)


###create data from cor####
for (i in 1:length(matr)){
matr[[i]] <- mvrnorm(mu = rep(0, ncol(matr[[1]])),
                     n = 300, 
                     empirical = TRUE, 
                     Sigma = matr[[i]])
}

###name Variables of matrices####
for (i in 1:length(matr)){
  colnames(matr[[i]]) <- str_c("V", 1:ncol(matr[[i]]))
  #rownames(matr[[i]]) <- str_c("V", 1:ncol(matr[[i]]))
}


###matrix -> data frame####
for (i in 1:length(matr)){
  matr[[i]] <- as.data.frame(matr[[i]])
}

####Lavaan function####

fitmeasures(testlist)

testlist <- cfaList(model, matr)

testlist <- runMI(model, matr, fun = "sem")

testlist@test

####trycatch version#####
get_ind <- function(model, matr, n){
  a = c()
  b = c()
  c = c()
  for (i in 1:length(matr)){
    tryCatch({
    cfa(model, sample.cov = matr[[i]], sample.nobs = n) %>% 
      fitMeasures(c("rmsea", "cfi", "srmr")) %>% 
      as.vector() -> x
    a[i] <- x[1]
    b[i] <- x[2]
    c[i] <- x[3]
    }, 
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")
    })
  return(ind <- data.frame(rmsea = a,
                    cfi = b,
                    srmr = c))
  }
}


###better version#####

get_ind <- function(model, matr, n){
  a = c()
  b = c()
  c = c()
  for (i in 1:length(matr)){
      x <- cfa(model, sample.cov = matr[[i]], sample.nobs = n)
      if (lavInspect(x, "converged") == TRUE){
        fitMeasures(x, c("rmsea", "cfi", "srmr")) %>% 
        as.vector() -> x
      a[i] <- x[1]
      b[i] <- x[2]
      c[i] <- x[3]
      }else{
        a[i] <- NA
        b[i] <- NA
        c[i] <- NA
      }
  }
  return(ind <- data.frame(rmsea = a,
                    cfi = b,
                    srmr = c))
}

###lavaan model####
model <- '
V2 ~ V1+V3
V1 ~~ 0*V3
'

dat_ind <- get_ind(model, matr, 200)


#####plots####
dat_ind %>% ggplot() + geom_density(aes(x = rmsea)) +
  coord_cartesian(xlim = c(0,1)) + scale_x_continuous(breaks = seq(0,1,0.1))

dat_ind %>% ggplot() + geom_histogram(aes(x = srmr), binwidth = .1) +
  coord_cartesian(xlim = c(0,1)) + scale_x_continuous(breaks = seq(0,1,0.1))

dat_ind %>% ggplot() + geom_histogram(aes(x = cfi), binwidth = .1) +
  coord_cartesian(xlim = c(0,1)) + scale_x_continuous(breaks = seq(0,1,0.1))

dat_ind %>% filter(rmsea < .08) %>% nrow()

