# Helper function to check the weights (MAYBE ADD SOME MORE COMMENTS)
check_weights <- function(dep_var, weights) {
  if(is.null(weights)) weights <- rep(1, length(dep_var))
  weights[is.na(weights)] <- 0
  if(!is.numeric(weights)) stop(msg = "\"Weights\" must be a numeric vector!")
  if(!length(dep_var) == length(weights)) stop(msg = "The input vector \"weights\" must be of equal length as the input vector \"dep_var\"!")
  if(!all(weights >= 0)) stop(msg = "Weights cannot contain negative values!")
  if(all(weights == 0)) stop(msg = "Not all weights can be set to zero or NA!")
  return(weights)
}
