### Rifreg coefficient plot 1
plot.rifreg <- function(x, varselect=NULL, alpha=0.05){

  if(length(x$tau)==1|x$rif!="quantile"){
    stop("Can only plot coefficients for a sequence of quantile RIF regressions, i.e. length(tau)>1.")
  }

  estimates <- as.data.frame(x$estimates)
  if(x$rif=="quantile"){
    names(estimates) <- x$tau
  }
  estimates$variable <- rownames(estimates)
  estimates <- as.data.frame(tidyr::pivot_longer(estimates,-variable,names_to="tau"))

  if(is.null(x$bootstrap_se)){
    estimates$se <- NA
  }else{
    bootstrap_se <- as.data.frame(x$bootstrap_se)
    if(x$rif=="quantile"){
      names(bootstrap_se) <- x$tau
    }
    bootstrap_se$variable <- rownames(bootstrap_se)
    bootstrap_se <- as.data.frame(tidyr::pivot_longer(bootstrap_se,-variable,names_to="tau"))
    estimates$se <- bootstrap_se$value
  }
  estimates$tau <- as.numeric(estimates$tau)

  variables <- unique(estimates$variable)
  if(is.null(varselect)){
    varselect <- variables
  }
  if(is.numeric(varselect)){
    if(sum(varselect %in% 1:length(variables))==length(varselect)){
      varselect <- variables[varselect]
    }else{
      varselect <- variables
    }
  }
  if(sum(varselect %in% variables)!=length(varselect)){
    varselect <- variables
  }

  df <- subset(estimates, variable %in% varselect)
  t <-  qnorm(1-alpha/2)

  #Actual plot
  if(length(varselect)==1){
    plot <- ggplot(df, aes(tau,value, color=variable, fill=variable)) +
      geom_hline(yintercept = 0, colour="grey") +
      geom_point() + geom_line() +
      labs(title=paste("Rifreg coefficients",varselect,sep=": "), y="coefficient", x="tau")
  } else {
    plot <- ggplot(df, aes(tau,value, color=variable, fill=variable)) +
      geom_hline(yintercept = 0, colour="grey") +
      geom_point() + geom_line() +
      facet_wrap( ~ variable, scales="free") +
      labs(title="Rifreg coefficients", y="coefficient", x="tau")
  }
  if(is.null(x$bootstrap_se)==FALSE){
    plot <- plot + geom_ribbon(aes(ymin = value - t*se, ymax = value + t*se), alpha=0.4, color=NA)
  }
  print(plot)
}
