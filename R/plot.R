#' Plot Coefficients for an \code{rifreg} Object
#'
#' Coefficients are plotted for each quantile and each covariate.
#' Specific covariates can be selected and SE can also be displayed.
#'
#' @param x an object of class "rifreg", usually, a result of a call to [rifreg()] with code{functional = "quantiles"}.
#' @param varselect vector of length 1 or more containig the names of the covariates to display.
#' @param alpha numeric value between 0 and 1 (default = 0.05) that defines the confidence interval
#'              plotted as a ribbon and defined as \code{qnorm(1-alpha/2)} * standard error.
#'              Only required if bootstrapped standard errors where computed.
#' @param ... other parameters to be passed through to plotting functions.
#'
#' @return a ggplot containing the coefficients for each (selected) covariate
#' @export
#'
#' @examples
#'
#' data <- CPSmen8305[1:300,]
#' rifreg <- est_rifreg(formula = log(wage) ~ union + age,
#'                      data = data,
#'                      functional = "quantiles",
#'                      custom_functional = NULL,
#'                      quantiles = seq(0.1, 0.9, 0.1),
#'                      weights = weights,
#'                      bootstrap = FALSE,
#'                      bootstrap_iterations = 100,
#'                      cores = 1,
#'                      model = TRUE)
#'
#' plot(rifreg)
#'
#' plot(rifreg, varselect = c("age", "unionyes"), alpha = 0.1)
#'
plot.rifreg <- function(x, varselect = NULL, alpha = 0.05, ...){

  estimates <- as.data.frame(x$estimates)

  if(x$functional=="quantiles") {
    if(length(x$quantiles)==1) stop("Can only plot coefficients for a sequence of quantile RIF regressions, i.e. length(quantiles)>1.")
    names(estimates) <- x$quantiles
  }


  estimates$variable <- rownames(estimates)
  estimates <- as.data.frame(tidyr::pivot_longer(estimates,-variable,names_to="quantiles"))

  if(is.null(x$bootstrap_se)) {
    estimates$se <- NA
  }
  else {
    bootstrap_se <- as.data.frame(x$bootstrap_se)
    if(x$functional=="quantiles"){
      names(bootstrap_se) <- x$quantiles
    }
    bootstrap_se$variable <- rownames(bootstrap_se)
    bootstrap_se <- as.data.frame(tidyr::pivot_longer(bootstrap_se,-variable,names_to="quantiles"))
    estimates$se <- bootstrap_se$value
  }
  estimates$quantiles <- as.numeric(estimates$quantiles)

  variables <- unique(estimates$variable)
  if(is.null(varselect)){
    varselect <- variables
  }
  if(is.numeric(varselect)){
    if(sum(varselect %in% 1:length(variables))==length(varselect)){
      varselect <- variables[varselect]
    }
    else {
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
    plot <- ggplot(df, aes(quantiles,value, color=variable, fill=variable)) +
      geom_hline(yintercept = 0, colour="grey") +
      geom_point() + geom_line() +
      labs(title=paste("Rifreg coefficients",varselect,sep=": "), y="coefficient", x="quantiles")
  } else {
    plot <- ggplot(df, aes(quantiles,value, color=variable, fill=variable)) +
      geom_hline(yintercept = 0, colour="grey") +
      geom_point() + geom_line() +
      facet_wrap( ~ variable, scales="free") +
      labs(title="Rifreg coefficients", y="coefficient", x="quantiles")
  }
  if(!is.null(x$bootstrap_se)){
    plot <- plot + geom_ribbon(aes(ymin = value - t*se, ymax = value + t*se), alpha=0.4, color=NA)
  }

  print(plot)
}