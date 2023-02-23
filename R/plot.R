#' Plot the coefficients of a \code{rifreg} object
#'
#' Coefficients are plotted for each quantile and each covariate.
#' Specific covariates can be selected and standard errors displayed if desired.
#'
#' @param x an object of class "rifreg", usually, a result of a call to \link[rifreg]{rifreg} with \code{statistic = "quantiles"}.
#' @param varselect vector of length 1 or more containig the names of the covariates to display.
#' @param confidence_level numeric value between 0 and 1 (default = 0.95) that defines the confidence interval
#'              plotted as a ribbon and defined as \code{qnorm(confidence_level/2)} * standard error.
#' @param vcov Function to estimate covariance matrix of rifreg coefficients if covariance matrix has not been bootstrapped.
#'             Per default, heteroscedasticity-consistent (HC) standard errors are calculated using \link[sandwich]{sandwich}.
#'             Note: These standard errors do not take the variance introduced by estimating RIF into account.
#' @param ... other parameters to be passed to plotting function. See \link[ggplot2]{ggplot} for further information.
#'
#' @return a "ggplot" containing the coefficients for each (selected) covariate
#' @export
#'
#' @examples
#'
#' rifreg <- rifreg(
#'   formula = log(wage) ~ union +
#'     nonwhite +
#'     married +
#'     education +
#'     experience,
#'   data = men8385,
#'   statistic = "quantiles",
#'   probs = seq(0.1, 0.9, 0.1),
#'   weights = weights
#' )
#'
#' plot(rifreg)
#'
#' plot(rifreg, varselect = c("age", "unionyes"), confidence_level = 0.1)
#'
plot.rifreg <- function(x, varselect = NULL, confidence_level = 0.05, vcov = sandwich::sandwich, ...) {
  estimates <- as.data.frame(x$estimates)

  if (is.null(x$bootstrap_se)) {
    warning("Standard errors have not been bootstrapped!\nAnalytical s.e. do not take variance introduced by\nestimating the RIF into account.")
    standard_errors <- as.data.frame(do.call("cbind", lapply(lapply(x$rif_lm, vcov, ...), function(x) sqrt(diag(x)))))
  } else {
    standard_errors <- as.data.frame(x$bootstrap_se)
  }

  if (x$statistic == "quantiles") {
    names(estimates) <- x$probs
    estimates$variable <- rownames(estimates)
    estimates <- reshape(estimates,
      idvar = "variable",
      ids = estimates$variable,
      times = setdiff(names(estimates), "variable"),
      timevar = "probs",
      varying = list(setdiff(names(estimates), "variable")),
      direction = "long",
      v.names = "value"
    )

    estimates$probs <- as.numeric(estimates$probs)
    names(standard_errors) <- x$probs
    standard_errors$variable <- rownames(standard_errors)
    standard_errors <- reshape(standard_errors,
      idvar = "variable",
      ids = standard_errors$variable,
      times = setdiff(names(standard_errors), "variable"),
      timevar = "probs",
      varying = list(setdiff(names(standard_errors), "variable")),
      direction = "long",
      v.names = "value"
    )
  } else {
    estimates$variable <- rownames(estimates)
    standard_errors$variable <- rownames(standard_errors)
    estimates <- reshape(estimates,
      idvar = "variable",
      ids = estimates$variable,
      times = setdiff(names(estimates), "variable"),
      varying = list(setdiff(names(estimates), "variable")),
      direction = "long",
      v.names = "value"
    )
    standard_errors <- reshape(standard_errors,
      idvar = "variable",
      ids = standard_errors$variable,
      times = setdiff(names(standard_errors), "variable"),
      varying = list(setdiff(names(standard_errors), "variable")),
      direction = "long",
      v.names = "value"
    )
  }

  estimates$se <- standard_errors$value
  variables <- unique(estimates$variable)

  if (is.null(varselect)) {
    varselect <- variables
  }
  if (is.numeric(varselect)) {
    if (sum(varselect %in% 1:length(variables)) == length(varselect)) {
      varselect <- variables[varselect]
    } else {
      varselect <- variables
    }
  }
  if (sum(varselect %in% variables) != length(varselect)) {
    varselect <- variables
  }

  df <- subset(estimates, variable %in% varselect)
  t <- qnorm(confidence_level / 2)

  if (x$statistic == "quantiles" & !length(x$probs) == 1) {
    plot <- ggplot(df, aes(probs, value, color = variable, fill = variable)) +
      geom_hline(yintercept = 0, colour = "grey") +
      geom_point() +
      geom_line() +
      geom_ribbon(aes(ymin = value - t * se, ymax = value + t * se), alpha = 0.3, color = NA) +
      facet_wrap(~variable, scales = "free") +
      labs(y = "coefficient", x = "probs")

    if (length(varselect) == 1) {
      plot <- plot + theme(legend.position = "none")
    }
  } else {
    plot <- ggplot(
      df,
      aes(x = variable, y = value)
    ) +
      geom_hline(
        yintercept = 0,
        colour = gray(1 / 2), lty = 2
      ) +
      geom_point(aes(
        x = variable,
        y = value
      )) +
      geom_linerange(
        aes(
          x = variable,
          ymin = value - t * se,
          ymax = value + t * se
        ),
        linewidth = 1
      ) +
      coord_flip()
  }
  print(plot)
}
