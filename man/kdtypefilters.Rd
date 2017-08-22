\name{kdtypefilters}
\alias{kdtypefilters}

\title{Kdtypefilters Function}

\description{Function to make different type of filters to time series(mean, median, max, min, gauss, rect).}

\usage{
kdtypefilters <- function (TSerie, WidthW=10)
}

\arguments{
\item{TSerie}{Time Series.}
\item{Width}{Windows width.}

}

\value{This function filter a time series using a slide window. Creates a matrix as each column of this one stores a time series filtered by a different filter (gauss, mean, median, max and min)}

\author{
Francisco Javier Martinez de Pisón \email{francisco.martinez@dim.unirioja.es}
Miguel Lodosa Ayala                \email{miguelodosa@hotmail.com}\cr
}


\references{
}

\seealso{
}
\keyword{}

