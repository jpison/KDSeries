\name{kdplotseries}
\alias{kdplotseries}

\title{kdplotseries Function}

\description{Function to draw the time series.}

\usage{
kdplotseries <- function(Series, Num=4,Ini=0,End=dim(Series)[1], Colors=1:dim(Series)[2])
}

\arguments{
\item{Series}{Matrix with Time Series (Each Column is one Time Series).}
\item{Num}{Num of Horizontal plots.}
\item{Ini}{First point.}
\item{End}{Last point.}
\item{Colors}{Color for each Time Series.}

\value{This function plot in the same frame four filtered (different width window) series but the graphiph is divided in four parts so we can look it easily.}
\author{
Francisco Javier Martinez de Pisón. \cr\email{francisco.martinez@dim.unirioja.es}\cr

Miguel Lodosa Ayala.                 \cr\email{miguelodosa@hotmail.com}\cr
}

\references{
}

\seealso{
}
\keyword{}

