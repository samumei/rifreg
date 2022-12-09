summary.rifreg <- function(x){
  estimates <- x$estimates #do.call("cbind",lapply(x$rif_lm, coef))
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
