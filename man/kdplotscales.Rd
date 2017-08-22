\name{kdplotscales}
\alias{kdplotscales}

\title{kdplotscales Function}

\description{}

\usage{
kdplotscales <- function(MAT, Positions=1:dim(MAT$MFilt)[1],Ini=1,End=dim(MAT$MFilt)[2])
}

\arguments{
\item{MAT}{Matrix with Differents Filters(from kdmatfilter())}
\item{Positions}{Plot filter placed in Positions.}
\item{Ini}{First point, usually by defect.}
\item{End}{Final point, usually by defect.}
}

\value{This function represent vertical lines in a plot so x axis is time and y axis are the different kind of filter (different window width).}
\author{
Francisco Javier Martinez de Pisón. \cr\email{francisco.martinez@dim.unirioja.es}\cr

Miguel Lodosa Ayala.                 \cr\email{miguelodosa@hotmail.com}\cr
}

\references{
}

\seealso{
}
\keyword{}

