#' Sample of male wage data from the CPS 1983-1985
#'
#' @description A sample of the the Merged Outgoing Rotation Group of the
#' Current Population Survey of 1983, 1984 and 1985 used by
#' Firpo, Fortin & Lemieux (2009). The data contains a selection of 10 variables
#' and a sample of 26,695 observations of male workers -- corresponding to a tenth of the
#' original 266,956 observations. See Lemieux (2006) for details on data selection and recoding.
#'
#' @format A data frame with 26,695 rows and 10 variables.
#' \describe{
#'   \item{wage}{Hourly wage in US dollars at constant prices}
#'   \item{union}{Union status indicator}
#'   \item{nonwhite}{Non-white indicator}
#'   \item{married}{Married indicator}
#'   \item{education}{Factor variable with 6 education levels: high-school graduates (reference), elementary, high-school dropouts , some college, college graduates, post college graduates}
#'   \item{experience}{Factor variable with 9 potential experience levels, each of five years gap, 20 to 24 years as reference level)}
#'   \item{weights}{CPS sample weights}
#'   \item{age}{Age in years}
#'   \item{education_in_years}{Education in years}
#'   \item{experience_in_years}{Experience in years}
#' }
#'
#' @source
#'   Sergio Firpo, Nicole M. Fortin, and Thomas Lemieux, "Unconditional
#'   Quantile Regressions", Econometrica, Vol. 77, No. 3 (May, 2009), pp.
#'   953-973.
#'
#'   Replication files:
#'   <https://www.econometricsociety.org/publications/econometrica/2009/05/01/unconditional-quantile-regressions>
#'
#'   Thoms Lemieux, "Increasing Residual Wage Inequality: Composition Effects,
#'   Noisy Data, or Rising Demand for Skill?", American Economic Review, Vol. 96,
#'   No. 3 (June, 2006), pp. 461-498.
"men8385"
