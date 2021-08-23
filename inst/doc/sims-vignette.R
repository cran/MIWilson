## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(MIWilson)

## -----------------------------------------------------------------------------
#creating missing data
create_missing_data <- function(n, p, m, MIA_perc) {
  
  complete = incomplete = rbinom(n, 1, p)
  
  #setting up number of missing values, dataset with missing values
  blanks = floor(MIA_perc * n)
  idcs = 1:length(complete)
  incomplete[sample(idcs, blanks)] = NA
  
  return(incomplete)

}


#creating multiple imputations
create_imps <- function(n, m, incomplete) {
  
  count_one = table(incomplete)[2]
  count_zero = table(incomplete)[1]
  
  imputations = matrix(nrow = n, ncol = m)
  for (i in 1:m) {
    p_star = rbeta(1, count_one + 1, count_zero + 1)
    incomp_idx = which(is.na(incomplete))
    
    curr_imp = incomplete
    curr_imp[incomp_idx] = rbinom(length(incomp_idx), 1, p_star)
    
    imputations[,i] = curr_imp
  }
  
  return(imputations)
  
}


## -----------------------------------------------------------------------------
n = 100
p = 0.7
m = 10
MIA_perc = 0.3

incomplete = create_missing_data(n, p, m, MIA_perc)
imputations = create_imps(n, m, incomplete)

phats = colSums(imputations)/nrow(imputations)
mi_wald_phat(phats = phats, n = nrow(imputations))
mi_wilson_phat(phats = phats, n =nrow(imputations))

