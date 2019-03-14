if(!require("pacman"))install.packages("pacman")
pacman::p_load("tidyverse", "clusterGeneration")

#----gen_matrices----
gen_mat <- function(dim){
  out <- genPositiveDefMat(dim,
                           covMethod = "onion",
                           alphad = 1L,
                           rangeVar = c(1,10))
  out <- out$Sigma
  colnames(out) <- str_c("V", seq_len(dim))
  out
}
gen_mats <- function(dim, n){
  map(rep.int(as.integer(dim), as.integer(n)), gen_mat)
}

mats <- tibble(dim = rep(c(3, 6, 9), each = 30))
mats <- mutate(mats, mat = map(dim, gen_mat))
