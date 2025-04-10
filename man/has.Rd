% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/has_methods.R
\name{has}
\alias{has}
\alias{has,ATO-method}
\title{Generic to check if an ATO has data in a given slot}
\usage{
has(object, value, error = FALSE)

\S4method{has}{ATO}(object, value, error = FALSE)
}
\arguments{
\item{object}{an \code{\link{ATO}}}

\item{value}{a vector with the names of the requested slots}

\item{error}{Should the code execution stop if the requested slots are empty?}
}
\value{
TRUE if the slots have data, FALSE if they're empty and
error = FALSE. Throws an error if the slots are empty and error = TRUE.
}
\description{
Generic to check if an ATO has data in a given slot
}
