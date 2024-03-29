% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anova_tables.R
\name{anova_tables}
\alias{anova_tables}
\title{ANOVA Tables}
\usage{
anova_tables(
  x,
  eta_squared = TRUE,
  omega_squared = TRUE,
  epsilon_squared = FALSE,
  effects = "fixed",
  contrast = NULL,
  at = NULL,
  standardized = TRUE,
  unstandardized = TRUE,
  ci = 0.95,
  ci_method = NULL,
  p_adjust = "none",
  bootstrap = FALSE,
  iterations = NULL,
  pbkrtest.limit = NULL,
  lmerTest.limit = NULL,
  digits = 3,
  id_col = "Subject",
  print = TRUE
)
}
\arguments{
\item{x}{an lmer model object}

\item{eta_squared}{logical. Include partial-eta sqaured effect size?
Default: TRUE}

\item{omega_squared}{logical. Include omega sqaured effect size?
Default: TRUE}

\item{epsilon_squared}{logical. Include epsilon sqaured effect size?
Default: FALSE}

\item{effects}{"fixed" or "all". default is "fixed" to reduce computation
time}

\item{contrast}{The factor(s) at which to compare levels at}

\item{at}{Additional interacting factor(s) to compare the effect of contrast
at}

\item{standardized}{Logical, indicating whether or not to print standardized
estimates. Standardized estimates are based on "refit" of the model
on standardized data but it will not standardize categorical predictors.
Defualt is TRUE.}

\item{unstandardized}{Logical, indicating whether or not to print
unstandardized estimates. Default is TRUE.}

\item{ci}{Confidence Interval (CI) level. Default to 0.95}

\item{ci_method}{Documention based on ?parameters::parameters.
Method for computing degrees of freedom for confidence
intervals (CI) and the related p-values. Allowed are following options
(which vary depending on the model class): "residual", "normal",
"likelihood", "satterthwaite", "kenward", "wald", "profile", "boot",
"uniroot", "ml1", "betwithin", "hdi", "quantile", "ci", "eti", "si",
"bci", or "bcai". See section Confidence intervals and approximation of
degrees of freedom in model_parameters() for further details.
When ci_method=NULL, in most cases "wald" is used then.}

\item{p_adjust}{The p-values adjustment method for frequentist multiple
comparisons. Can be one of "holm", "tukey", "hochberg",
"hommel", "bonferroni", "BH", "BY", "fdr" or "none" (default). See the
p-value adjustment section in the emmeans::test documentation.}

\item{bootstrap}{Documention based on ?parameters::parameters.
Should estimates be based on bootstrapped model? If TRUE, then arguments
of Bayesian regressions apply (see also bootstrap_parameters()).}

\item{iterations}{Documention based on ?parameters::parameters.
The number of bootstrap replicates. This only apply in the case of
bootstrapped frequentist models.}

\item{pbkrtest.limit}{Optional parameter that can be set to help calculate
dfs.
If you need to use this a warning message will appear in the console
telling you what to set this at.}

\item{lmerTest.limit}{Optional parameter that can be set to help calculate
dfs.
If you need to use this a warning message will appear in the console
telling you what to set this at.}

\item{digits}{How many decimal places to round to? Default is 3.}

\item{id_col}{The column containing subject ids. Default is "Subject"}

\item{print}{Create a knitr table for displaying as html table?
(default = TRUE)}
}
\description{
Get ANOVA F-Table, contrasts, and pairwise comparisons
}
