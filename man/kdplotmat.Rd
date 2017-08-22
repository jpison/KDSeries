\name{kdplotmat}
\alias{kdplotmat}

\title{kdplotmat Function}

\description{Function to draw the time series.}

\usage{
kdplotmat <- function(MAT, Positions=1:dim(MAT$MFilt)[1],Ini=1,End=dim(MAT$MFilt)[2])
}

\arguments{
\item{MAT}{Matrix with Differents Filters(from kdmatfilter()).}
\item{Positions}{Plot filter placed in Positions.}
\item{Ini}{First point.}
\item{End}{Last Point.}
}

\value{This function plot filtered time series (series has been filtered with different width windows).}
\author{
Francisco Javier Martinez de Pisón. \cr\email{francisco.martinez@dim.unirioja.es}\cr

Miguel Lodosa Ayala.                 \cr\email{miguelodosa@hotmail.com}\cr
}

\references{
}

\seealso{
}
\keyword{}

