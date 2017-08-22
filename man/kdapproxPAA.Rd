\name{kdapproxPAA}
\alias{kdapproxPAA}

\title{kdapproxPAA Function}

\description{Approximation to the mean values of the time series.}

\usage{
kdapproxPAA <- function (TSerie,len=length(TSerie)/15)
}

\arguments{
\item{TSerie}{Time Series.}
\item{len}{length of the sections.}
}

\value{This function approachs the values of a time series to the mean values by sections.}
\author{
Francisco Javier Martinez de Pisón. \cr\email{francisco.martinez@dim.unirioja.es}\cr

Miguel Lodosa Ayala.                 \cr\email{miguelodosa@hotmail.com}\cr
}


\references{
Keogh 2000
}

\seealso{
kdapproxPCA
kdapproxAPCA
}
\keyword{}

