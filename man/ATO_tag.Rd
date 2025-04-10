% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ATO_class.R
\docType{class}
\name{ATO_tag}
\alias{ATO_tag}
\alias{.ATO_tag}
\title{S4 class: ATO_tag}
\format{
A data frame with 0 rows and 12 variables:
\describe{
\item{manufacturer}{Manufacturer of the transmitter, character.}
\item{model}{Model of the transmitter, character.}
\item{power_level}{Power level of the transmitter, real.}
\item{ping_rate}{Expected ping rate of the transmitter, numeric. In seconds.}
\item{ping_variation}{Range of the variation added between pings, to reduce tag collisions, numeric. In seconds}
\item{serial}{Serial number of the tag, integer.}
\item{transmitter}{Transmitter code, character.}
\item{activation_datetime}{date and time of the tag activation, posixct.}
\item{battery_life}{expected battery duration of the tag, numeric.}
\item{sensor_type}{Type of sensor data associated with the transmitter, character.}
\item{sensor_unit}{Unit of the data associated with the transmitter, character.}
\item{animal}{Name of the animal that received the tag, character.}
}

An object of class \code{ATO_tag} (inherits from \code{data.frame}) with 0 rows and 12 columns.
}
\usage{
.ATO_tag
}
\description{
An S4 class for the \code{\link{ATO}} tags (@tag).
Does not contain internal slots. It is a table of
either data.frame, data.table, or tibble format.
}
\details{
The prototype @tag slot
contains the standard columns of the @tag slot.
Other columns are allowed, but these are necessary,
and must be of the designated types.

The tag slot contains information on the different transmitters being tracked.
Tags with multiple transmitter codes are coded as multiple rows associated
with a single serial number and a single animal.

Can be of type data.frame, data.table, or tibble,
depending on the @tbl slot.
See \code{\link{table_type}} for more details on
\code{\link{ATO}} table types.
}
\seealso{
make_tag
}
\keyword{classes}
\keyword{datasets}
