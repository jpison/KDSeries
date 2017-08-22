\name{kdfilterFFT}
\alias{kdfilterFFT}

\title{kdfilterFFT Function}

\description{Filter a Time Series by FFT function.}

\usage{
kdfilterFFT <- function (TSerie, WidthW=0.02*length(TSerie), Slide=WidthW , Filter="mean" , Range=c(20,70))
}

\arguments{
\item{TSerie}{Time Series.}
\item{WidthW}{Filter's Window Width}
\item{Slide}{Displacement of the window filter.}
\item{Filter}{Type of Filter. "mean"=rectangular window, "gauss"=gauss window.}
\item{Range}{Range of frecuencies that we want to eliminate in % (min % of total length,max % of total length).}
}

\value{This function makes the FFT filter of a time series.You can choose the Width of the filter window and the window slide.The window can be "gauss" type or "rectangular".Plot the original time series and the filtered series too.}
\author{
Francisco Javier Martinez de Pisón. \cr\email{francisco.martinez@dim.unirioja.es}\cr

Miguel Lodosa Ayala.                 \cr\email{miguelodosa@hotmail.com}\cr
}


\references{
}

\seealso{
}
\keyword{}

