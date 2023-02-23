################################################################################
## Load original STATA data "men8385.dta" used by FFL (2009)
## and prepare data set for export to rifreg package

## -----------------------------------------------------------------------------
## Description by Baltagi/Gosh (2017, J of Applied Econometrics):

# Firpo, Fortin, Lemieux (2009) replication data
#
# The data file and the original programs in STATA used to obtain the
# results in the paper may be found at
#
# https://www.econometricsociety.org/publications/econometrica/2009/05/01/unconditional-quantile-regressions
#
# Additional programs to compute more general RIF-regressions are
# available on the web site: http://www.econ.ubc.ca/nfortin/
#
#   The data file is men8385.dta, a STATA10 data file containing an
# extract of variables from the Merged Outgoing Rotation Group of the
# Current Population Survey of 1983, 1984 and 1985. The file contains
# 266956 observations on males with 17 variables whose definition is
# given by the variable labels. More detail about the data selection and
# recoding (e.g. top coding, wage deflator, etc.) is found in Lemieux
# (2006).
#
# The file men8385.dta is zipped in ffl-dta.zip. The same data may be
# found in the ASCII text file men8385.txt, which is zipped in
# ffl-txt.zip. Since men8385.txt is in DOS format, Unix/Linux users
# should use "unzip -a".
#
# The files men8385.dta and men8385.txt contain 266,956 observations on
# males and have 17 variables. The dependent variable is the logarithm
# of hourly wage from the current population survey outgoing rotation
# samples for 1983, 1984 and 1985. The independent variables are
# workers' demographic characteristics, which consist of five education
# categories (dropout, high-school graduates, some college, college
# graduates, post college graduates), nine potential experience
# categories (each of five years gap), and dummy variables for union,
# married, and nonwhite.

## -----------------------------------------------------------------------------

library("readstata13")
library("dplyr")

# Load data
# url <- "https://www.econometricsociety.org/publications/econometrica/2009/05/01/unconditional-quantile-regressions/supp/6822_data%20and%20programs_0.zip"
# download.file(url, destfile="6822_data%20and%20programs_0.zip")
# men8385 <- read.dta13(unz("6822_data%20and%20programs_0.zip","men8385.dta"))
men8385 <- read.dta13("data-raw/men8385.dta")

# Save dummies as factor variables
# nine potential experience categories (each of five years gap)
men8385$experience <- 5
men8385[which(men8385$ex1 == 1), "experience"] <- 1
men8385[which(men8385$ex2 == 1), "experience"] <- 2
men8385[which(men8385$ex3 == 1), "experience"] <- 3
men8385[which(men8385$ex4 == 1), "experience"] <- 4
# 5 = reference group
men8385[which(men8385$ex6 == 1), "experience"] <- 6
men8385[which(men8385$ex7 == 1), "experience"] <- 7
men8385[which(men8385$ex8 == 1), "experience"] <- 8
men8385[which(men8385$ex9 == 1), "experience"] <- 9

# Education
men8385$education <- 2
men8385[which(men8385$ed0 == 1), "education"] <- 0
men8385[which(men8385$ed1 == 1), "education"] <- 1
# high school = reference group
men8385[which(men8385$ed3 == 1), "education"] <- 3
men8385[which(men8385$ed4 == 1), "education"] <- 4
men8385[which(men8385$ed5 == 1), "education"] <- 5

men8385$education <- as.character(men8385$education)
men8385$experience <- as.character(men8385$experience)
men8385$experience <- dplyr::recode_factor(men8385$experience,
  "5" = "20-24",
  "1" = "0-4",
  "2" = "5-9",
  "3" = "10-14",
  "4" = "15-19",
  "6" = "25-29",
  "7" = "30-34",
  "8" = "35-39",
  "9" = ">=40"
)

men8385$education <- dplyr::recode_factor(men8385$education,
  "2" = "High School",
  "0" = "Elementary",
  "1" = "HS dropout",
  "3" = "Some College",
  "4" = "College",
  "5" = "Post-graduate"
)


# Save log wage as wage hourly wage in dollars
men8385$wage <- exp(men8385$lwage)

# Rename/relevel remaining indicators
men8385$union <- as.factor(men8385$covered)
men8385$covered <- NULL

men8385$married <- as.factor(men8385$marr)
men8385$marr <- NULL
men8385$nonwhite <- as.factor(men8385$nonwhite)
levels(men8385$married) <- levels(men8385$nonwhite) <- levels(men8385$union) <- c("no", "yes")

# Rename weight and education in years variable
names(men8385)[names(men8385) == "eweight"] <- "weights"
names(men8385)[names(men8385) == "educ"] <- "education_in_years"
names(men8385)[names(men8385) == "exper"] <- "experience_in_years"

# Check experience and age groups
men8385 %>%
  group_by(experience) %>%
  dplyr::summarise(
    min = min(experience_in_years, na.rm = TRUE),
    max = max(experience_in_years, na.rm = TRUE)
  )

men8385 %>%
  group_by(education) %>%
  dplyr::summarise(
    min = min(education_in_years, na.rm = TRUE),
    max = max(education_in_years, na.rm = TRUE)
  )

# Select relevant variables
sel_vars <- c("wage", "union", "nonwhite", "married", "education", "experience", "weights", "age", "education_in_years", "experience_in_years")
men8385 <- men8385[, sel_vars]

# Save 20% sample of original data set
set.seed(123)
sel_obs <- sample(1:nrow(men8385), floor(nrow(men8385) / 5))
men8385 <- men8385[sel_obs, ]

usethis::use_data(men8385, overwrite = TRUE)
