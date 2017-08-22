\name{kdfilterWAVELET}
\alias{kdfilterWAVELET}

\title{kdfilterWAVELET function}

\description{Function that makes the wavelet function.}

\usage{
kdfilterWAVELET <- function (TSerie, Eliminate="Low")
}

\arguments{
\item{TSerie}{Original time series.}
\item{Eliminate}{Frecuencies that has to be eliminated.It can be "Low" or "High" it depends that the user wants to get}
}

\value{This function eliminates the frecuency components that the users want.For easily they can choose between 2 options.If the user wants to get the peaks of the time series, he usually has to filter the low frecuencies. }
\author{
Francisco Javier Martinez de Pisón. \cr\email{francisco.martinez@dim.unirioja.es}\cr

Miguel Lodosa Ayala.                 \cr\email{miguelodosa@hotmail.com}\cr
}

\references{
 http://www.atmos.washington.edu/~ealdrich/wavelets
"Automatic theories", Samir Kouoro R. and Rodrigo Musalem M.
}

\seealso{
kdfilterFFT
}

\keyword{neural}

