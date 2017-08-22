\name{kdapproxAPCA}
\alias{kdapproxAPCA}

\title{kdapproxAPCA Function}

\description{give back a reduced representation of the data.The data have been divided into 
variable length frames.}

\usage{
kdapproxAPCA <- function (TSerie,len=length(TSerie)/15)
}

\arguments{
\item{TSerie}{Time Series.}
\item{len}{length of the sections.}
}

\value{This function give back a list that contains two vectors.One of them contains the mean values of the frames that the.
function divides the time series and the another one contains the occupied space of each other.The function can be used for compressing a time series. The frames have different length, this way we lost some information but the memory space is reduced.
}

\author{
Francisco Javier Martinez de Pisón. \cr\email{francisco.martinez@dim.unirioja.es}\cr

Miguel Lodosa Ayala.                 \cr\email{miguelodosa@hotmail.com}\cr
}


\references{
Keogh and Michael J.Pazzani (California University)
Yi & Faloutsos
}

\seealso{
kdapproxPCA
kdapproxAPCA
}
\keyword{}

