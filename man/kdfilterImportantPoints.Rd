\name{kdfilterImportantPoints}
\alias{kdfilterImportantPoints}

\title{Filter function}

\description{Function that gives back the most important points of a function.}

\usage{
kdfilterImportantPoints <- function (TSerie, R,type="points")
}

\arguments{
\item{TSerie}{Time Series.}
\item{R}{rate compression.Values beetween 1 and 1.9}
\item{type}{Style important points plot.}
}

\value{This function returns a list that contains tree elements: the important points values, the positions of these ones, 
 and the compression rate (respect of the original series).The important points usually are the zerocrossing values (view zerocrossing function).}

\author{
Francisco Javier Martinez de Pisón. \cr\email{francisco.martinez@dim.unirioja.es}\cr

Miguel Lodosa Ayala.                 \cr\email{miguelodosa@hotmail.com}\cr
}


\references{
Pernia Espinoza, A.V. "TAO" \cr \cr
Simon Haykin. Neural Networks. A comprehensive foundation. 2nd Edition. \cr \cr
}

\seealso{
}

\keyword{neural}

