% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/promillo-nodoc.R
\name{get_permille}
\alias{get_permille}
\title{Permille}
\usage{
get_permille(alcohol_drunk, bodywater, drinking_time, units = "hours")
}
\arguments{
\item{alcohol_drunk}{a single numeric value of the alcohol mass in gram.}

\item{bodywater}{a single numeric value of body water.}

\item{drinking_time}{a POSIXct vector of length 2. First argument is the
time the person started to drink alcohol, second argument is the end of
drinking.}

\item{units}{a single value of type "character". Possible choices are "hours",
"mins".}
}
\value{
The function returns the permille value of a person after drinking for
more than one hour.
}
\description{
Internal function that calculates permille using alcohol mass, bodywater
and drinking time.
}
