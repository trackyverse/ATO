% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ATO_class.R
\docType{class}
\name{ATO_log}
\alias{ATO_log}
\alias{.ATO_log}
\title{S4 class: ATO_log}
\format{
A data frame with 0 rows and 4 variables:
\describe{
\item{datetime}{date and time of the log entry, posixct format.}
\item{package}{name of the package that hosts the function that made the log entry, character.}
\item{call}{function call that made the log entry, character.}
\item{log}{log entry, character.}
}

An object of class \code{ATO_log} (inherits from \code{data.frame}) with 0 rows and 4 columns.
}
\usage{
.ATO_log
}
\description{
An S4 class for the \code{\link{ATO}} log (@log).
Does not contain internal slots. It is a table of
either data.frame, data.table, or tibble format.
}
\details{
The prototype @log slot
contains the standard columns of the @log slot.
Other columns are not allowed.

Contains summary information of the actions performed throughout
the life of the ATO.

Can be of type data.frame, data.table, or tibble,
depending on the @tbl slot.
See \code{\link{table_type}} for more details on
\code{\link{ATO}} table types.
}
\keyword{classes}
\keyword{datasets}
