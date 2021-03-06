% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_analysis.R
\name{list_vars}
\alias{list_vars}
\alias{output_vars}
\alias{input_vars}
\title{List all output or input variables of a Stan model}
\usage{
output_vars(code = "", file = NULL, verbose = FALSE)

input_vars(code = "", file = NULL, verbose = FALSE)
}
\arguments{
\item{code}{Stan code as a string. Ignored if \code{file} is not NULL.}

\item{file}{File that contains Stan code. If this is
NULL, \code{code} is used as input.}

\item{verbose}{Should some informational messages be printed?}
}
\value{
transformed and prettified MIR code
}
\description{
List all output or input variables of a Stan model
}
\section{Functions}{
\itemize{
\item \code{output_vars}: Lists all parameters, transformed parameters and
generated quantities.

\item \code{input_vars}: Lists all data.
}}

