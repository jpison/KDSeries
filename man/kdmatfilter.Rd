\name{kdmatfilter}
\alias{kdmatfilter}

\title{kdmatfilter Function}

\description{Obtain a Matrix with Different Filters.}

\usage{
kdmatfilter <- function (TSerie, WidthWVect, Filter="gauss")
}

\arguments{
\item{TSerie}{Time Series.}
\item{WidthWVect}{vector that contains different Windows width.}
\item{Filter}{Filter type: 'gauss'=gaussian, 'median'=median, 'mean'=mean, 'min'=minimun number, 'max'=maximum number, 'rect'=rectangular}
}

\value{Give back a list that contains a matrix with the different filtered time series (each file is a different width window ).}
\author{
Francisco Javier Martinez de Pisón. \cr\email{francisco.martinez@dim.unirioja.es}\cr

Miguel Lodosa Ayala.                 \cr\email{miguelodosa@hotmail.com}\cr
}

\references{
}

\seealso{
}
\keyword{}

