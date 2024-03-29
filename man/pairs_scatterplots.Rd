% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pairs_scatterplots.R
\name{pairs_scatterplots}
\alias{pairs_scatterplots}
\title{Scatterplots for all variable pairs}
\usage{
pairs_scatterplots(data, x = c(), y = c(), all_pairs = FALSE)
}
\arguments{
\item{data}{dataframe}

\item{x}{X-axis variables}

\item{y}{Y-axis variables}

\item{all_pairs}{Logical. If TRUE then will overwrite x, and y parameters
and just print out all combinations of paris in the dataframe. (Default: FALSE)}
}
\description{
This function will print out scatterplots of all pairs of variables in a dataframe.
Histograms on x and y variables are displayed on the margins, using ggMarginal()
Correlation statistics are printed at the bottom left of figure.
}
