\name{kdfilter}
\alias{kdfilter}

\title{Filter function}

\description{Function to make different window filters (mean, median, max, min, gauss, rect).}

\usage{
train(net,P,T,n.epochs,g=adapt.NeuralNet, error.criterium="MSE", Stao=NA, report=TRUE, show.step)
}

\arguments{
\item{net}{Neural Network to train.}
\item{P}{Training set input values.}
\item{T}{Training set output values}
\item{n.epochs}{Number of epochs to train.}
\item{g}{Adaptative function used for training. The default provides a quicker C code version of the adaptative backpropagation with momentum method.}
\item{error.criterium}{Criterium used to measure the goodness of fit.}
\item{Stao}{Initial value of the S parameter used by the TAO algorithm.}
\item{report}{Logical value indicating whether the training function should keep quiet or should provide graphical/written information during the training process instead.}
\item{show.step}{If report is TRUE then a report is provided every show.step epochs.}
}

\value{This function returns a trained Neural Network object with weights and biases adjusted by the adaptative backpropagation with momentum method. The whole training set is considered.}
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

