if(!require("pacman"))install.packages("pacman")
pacman::p_load("tidyverse", "clusterGeneration", "lavaan", "tictoc", "furrr")

#----fit-cfa----
incessantly <- function(.f, quiet = TRUE){
  # a function in the spirit of purrr::safely, quitly and possibly
  # it is essentially a merge of safely & quitly
  # while safely does not quit on error,
  # but instead returns a list with error and result
  # it does not capture warnings/messages as does quitly, which fails on error...
  # so incessantly always returns a list
  # with result, output, error, warnings & messages
  # and never fails on error
  # however in contrast to safely and quietly it does not return NULL
  # that way one can build up a data.frame from the output
  # https://github.com/tidyverse/purrr/issues/120
  .f <- quietly(safely(.f, otherwise = NA, quiet = TRUE))
  function(...){
    out <- do.call(.f, list(...))
    out <- list(result = out$result$result,
                output = out$output,
                error = out$result$error,
                warnings = out$warnings,
                messages = out$messages)
    modify_if(out, ~length(.x) == 0L, ~"")
  }
}

fit_cfa <- function(mat){
  vars <- colnames(mat)
  model <- str_c("g =~ ", str_c(vars, collapse = " + "))
  fit <- incessantly(cfa)(model,
                      sample.cov = mat,
                      sample.nobs = 200
  )
  fit
}

fits <- mats %>% pull(mat) %>% map(fit_cfa) %>% transpose() %>% as_tibble()
fits <- mutate(mats, !!!fits)
