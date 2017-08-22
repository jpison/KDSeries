\name{kdextract}
\alias{kdextract}

\title{kdextract Function}

\description{Give back a list that contains different parameters.}

\usage{
kdextract <- function(TSerie, Param=c(rep(TRUE,13)), Threshold, LongMin=rep(0,13))
}

\arguments{
\item{TSerie}{Time Series.}
\item{Param}{Measures->INCREMENTAL,DECREMENTAL...}
\item{Threshold}{% of a range(TSerie).}
}

\value{Function that give back positions where time series inclination is positive, negative or zero(horizontal).Besides, distinguish between three rates of inclination (low, mediun or high).}
\author{
Francisco Javier Martinez de Pisón. \cr\email{francisco.martinez@dim.unirioja.es}\cr

Miguel Lodosa Ayala.                 \cr\email{miguelodosa@hotmail.com}\cr
}


\references{
}

\seealso{
}
\keyword{}

