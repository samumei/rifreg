#' summary method for class "rifreg"
#'
#' @param x an object of class "rifreg", usually , a result of a call to [rifreg()].
#'
#' @return the function \code{summary.rifreg()} returns a list of summary statistics derived from
#'  the fitted linear model given in object \code{x}. For further details see [stats::summary.lm()].
#' @export
#'
#' @examples
#'
#' data <- CPSmen8305[1:300,]
#' rifreg <- est_rifreg(formula = log(wage) ~ union + age,
#'                      data = data,
#'                      functional = "quantiles",
#'                      custom_functional = NULL,
#'                      quantiles = 0.5,
#'                      weights = weights,
#'                      bootstrap = FALSE,
#'                      bootstrap_iterations = 100,
#'                      cores = 1,
#'                      model = TRUE)
#'
#' summary(rifreg)
#'
summary.rifreg <- function(x){
  estimates <- x$estimates
  bootstrap_se <- x$bootstrap_se
  if(is.null(bootstrap_se)){
    bootstrap_se <- as.data.frame(matrix(rep(NA, prod(dim(estimates))),ncol=ncol(estimates)))
    names(bootstrap_se) <- names(estimates)
    rownames(bootstrap_se) <- rownames(estimates)
  }

  r.squared <- unlist(do.call("c",lapply(x$rif_lm, function(z) summary(z)[c("r.squared")])))
  adj.r.squared <- unlist(do.call("c",lapply(x$rif_lm, function(z) summary(z)[c("adj.r.squared")])))
  df <- do.call("c",lapply(x$rif_lm, function(z) z$df.residual))
  sigma <- unlist(do.call("c",lapply(x$rif_lm, function(z) summary(z)[c("sigma")])))

  for(i in 1:ncol(estimates)){
    cat("\n")
    cat("RIF regression coefficients for ", gsub("rif_","",colnames(estimates)[i]),"\n",sep="")
    cat("\n")
    res <- cbind(estimates[,i], bootstrap_se[,i])
    res <- cbind(res,res[,1]-res[,2]*1.96,res[,1]+res[,2]*1.96)
    colnames(res) <- c("Estimate","Bootstrap s.e.", "Lower bound", "Upper bound")
    print(res)
    cat("\n")
    cat("Residual standard error: ",sigma[i]," on ", df[i], " degrees of freedom\n", sep="")
    cat("Multiple R-squared: ",r.squared[i],",	Adjusted R-squared: ",adj.r.squared[i],"\n", sep="")
    cat("\n")
  }
}
