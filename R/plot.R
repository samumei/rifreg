#' Plot Coefficients for an \code{rifreg} Object
#'
#' Coefficients are plotted for each quantile and each covariate.
#' Specific covariates can be selected and SE can also be displayed.
#'
#' @param x an object of class "rifreg", usually, a result of a call to [rifreg()] with code{functional = "quantiles"}.
#' @param varselect vector of length 1 or more containig the names of the covariates to display.
#' @param confidence_level numeric value between 0 and 1 (default = 0.95) that defines the confidence interval
#'              plotted as a ribbon and defined as \code{qnorm(confidence_level/2)} * standard error.
#' @param vcov Function to estimate covariance matrix of rifreg coefficients if covariance matrix has not been bootstrapped.
#'             Per default, heteroscedasticity-consistent (HC) standard errors are calculated using [sandwich::sandwich]. Note: These
#'             standard errors do not take the variance introduced by estimating RIF into account.
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
#'                      probs = seq(0.1, 0.9, 0.1),
#'                      weights = weights,
#'                      bootstrap = FALSE,
#'                      bootstrap_iterations = 100,
#'                      cores = 1,
#'                      model = TRUE)
#'
#' plot(rifreg)
#'
#' plot(rifreg, varselect = c("age", "unionyes"), confidence_level = 0.1)
#'
plot.rifreg <- function(x, varselect = NULL, confidence_level = 0.05, vcov=sandwich::sandwich, ...){

  estimates <- as.data.frame(x$estimates)

  if(x$functional=="quantiles") {
    if(length(x$probs)==1) stop("Can only plot coefficients for a sequence of quantile RIF regressions, i.e. length(probs)>1.")
    names(estimates) <- x$probs
  }

  estimates$variable <- rownames(estimates)
  estimates <- as.data.frame(tidyr::pivot_longer(estimates,-variable,names_to="probs"))

  if(is.null(x$bootstrap_se)) {
    # estimates$se <- NA
    warning("Standard errors have not been bootstrapped!\nAnalytical s.e. do not take variance introduced by\nestimating the RIF into account.")
    standard_errors <- as.data.frame(do.call("cbind",lapply(lapply(x$rif_lm, vcov, ...), function(x) sqrt(diag(x)))))
  } else {
    standard_errors <- as.data.frame(x$bootstrap_se)
  }

  if(x$functional=="quantiles"){
      names(standard_errors) <- x$probs
  }
  standard_errors$variable <- rownames(standard_errors)
  standard_errors <- as.data.frame(tidyr::pivot_longer(standard_errors,-variable,names_to="probs"))
  estimates$se <- standard_errors$value

  estimates$probs <- as.numeric(estimates$probs)

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
  t <-  qnorm(confidence_level/2)

  #Actual plot
  plot <- ggplot(df, aes(probs,value, color=variable, fill=variable)) +
      geom_hline(yintercept = 0, colour="grey") +
      geom_point() +
      geom_line() +
      geom_ribbon(aes(ymin = value - t*se, ymax = value + t*se), alpha=0.3, color=NA) +
      facet_wrap( ~ variable, scales="free") +
      labs(y="coefficient", x="probs")
  if(length(varselect)==1){
  plot <- plot + theme(legend.position="none")
  }
  # if(!is.null(x$bootstrap_se)){
  #   plot <- plot +
  #     geom_ribbon(aes(ymin = value - t*se, ymax = value + t*se), alpha=0.4, color=NA)
  # }
  print(plot)
}
