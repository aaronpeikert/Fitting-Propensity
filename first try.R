pacman::p_load(matrixcalc, tidyverse, magrittr, clusterGeneration, lavaan, MASS)

####Matrices from Julia#####

setwd("C:/Users/Maximilian/Documents/Uni/Bachelorarbeit/Random Matrices")

test <- read_delim("test.txt", delim = ";", col_names = FALSE)

reformat_matrices <- function(data){
  d = sqrt(ncol(data))
  a = list(rep(1, nrow(data)))
  for (i in 1:nrow(data)){
    a[[i]] = matrix(as.double(data[i,]), ncol = d)
  }
  return(a)
}

matr <- reformat_matrices(test)



datp <- test %>% 
  gather(str_c("X", 1:ncol(test)), key = "varname", value = "cors")

datp %<>% filter(cors != 1)

datp %>% ggplot(aes(x = cors)) + geom_histogram(binwidth = .001)


####R function#####
generate_matrices <- function(d, k, n){
  a = list()
  for (i in 1:n){
    x = rcorrmatrix(d, k)
    a[[i]] = x
  }
  return(a)
}


###Plot####
test <- generate_matrices(5, 1, 200)

test <- unlist(test)

test <- as.data.frame(test)

test %<>% filter(test != 1)

test %>% ggplot(aes(x = test)) + geom_histogram(binwidth = .001)


###runif####

a = list()

for (i in 1:100000){
  x = runif(10, -1, 1)
  m = matrix(rep(1, 25), ncol = 5)
  m[1,2] = x[1]
  m[2,1] = x[1]
  m[1,3] = x[2]
  m[3,1] = x[2]
  m[1,4] = x[3]
  m[4,1] = x[3]
  m[1,5] = x[4]
  m[5,1] = x[4]
  m[2,3] = x[5]
  m[3,2] = x[5]
  m[2,4] = x[6]
  m[4,2] = x[6]
  m[2,5] = x[7]
  m[5,2] = x[7]
  m[3,4] = x[8]
  m[4,3] = x[8]
  m[3,5] = x[9]
  m[5,3] = x[9]
  m[4,5] = x[10]
  m[5,4] = x[10]
  if(is.positive.definite(m) == TRUE){
    a[[length(a)+1]] = m
  }else{}
}

a = unlist(a)

a = as.data.frame(a)

a %<>% filter(a != 1)

a %>% ggplot(aes(x = a)) + geom_histogram(binwidth = .001)
