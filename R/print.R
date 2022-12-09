### Print rifreg
print.rifreg <- function(x){
  estimates <- x$estimates #do.call("cbind",lapply(x$rif_lm, coef))
  cat("Rifreg coefficients:\n")
  print(estimates)
}



