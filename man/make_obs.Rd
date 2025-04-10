% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_functions.R
\name{make_obs}
\alias{make_obs}
\title{Make an ATO observations object}
\usage{
make_obs(
  animal = NA_character_,
  transmitter = NA_character_,
  type = NA_character_,
  terminal = NA,
  location = NA_character_,
  datetime = as.POSIXct(NA_real_),
  lat = NA_real_,
  lon = NA_real_,
  tz,
  ...
)
}
\arguments{
\item{animal}{Name of the animal that was observed, character.}

\item{transmitter}{Transmitter code that was observed, character.}

\item{type}{Type of observation (e.g. directly seen, manual tracking),
character.}

\item{terminal}{Was the animal permanently captured at the moment of
observation? logical.}

\item{location}{Name of the place where the observation occurred, character.}

\item{datetime}{date and time of the observation, posixct format.}

\item{lat}{latitude of the observation. Preferably in WGS84, numeric.}

\item{lon}{longitude of the observation. Preferably in WGS84, numeric.}

\item{tz}{the timezone of the datetime data.}

\item{...}{Non-standard columns to be added to the table.}
}
\value{
an ATO_obs object, ready to be used by \code{\link{add}} or
\code{\link{init_ato}}.
}
\description{
Formats the input data into the ATO format and appends the ATO_obs class.
}
\seealso{
ATO_obs
}
