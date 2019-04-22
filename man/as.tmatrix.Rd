\name{as.tmatrix}
\alias{as.tmatrix}
\alias{plot.tmatrix}
\alias{print.tmatrix}
\alias{summary.tmatrix}
\alias{plot.summary.tmatrix}
\alias{print.summary.tmatrix}
\title{
Population Transition Matrix
}
\description{
Convert a matrix to transition matrix (class \code{tmatrix}) and display some of its properties.
}
\usage{
as.tmatrix(x, names.st = NULL, ...)
\method{plot}{tmatrix}(x, ...)
\method{print}{tmatrix}(x, ...)
\method{summary}{tmatrix}(object, ...)
\method{print}{summary.tmatrix}(x, ...)

}
\arguments{
  \item{x}{For \code{as.tmatrix}, \code{x} is a square matrix,; for the plot,
                print and summary methods,\code{x} is a matrix of class \code{tmatrix}.}
  \item{object}{An object of class \code{tmatrix}, i.e, resulting from 
	\code{as.tmatrix}.}
  \item{names.st}{ A vector of names to rename the rows and columns of 
                           the transition matrix.}
  \item{\dots}{Other parameters passed to print and plot methods.}
}
\details{
  The purpose of \code{as.tmatrix} is to class a transition matrix as an object of
   class \code{tmatrix}, allowing the use of specific methods for transition
   matrices.
}
\value{
\code{as.tmatrix} returns a matrix of class \code{tmatrix}. 

\code{summary.tmatrix} prints the main parameters of the transition matrix: 
  the finite rate of increase ("lambda"), the stable stage distribution, the 
  reproductive value and the sensitivities and elasticities matrices. 
 
  \code{plot.tmatrix} draw barplots of the stable stage distribution and the 
  reproductive value and plot also a diagram of the life stage cycle.
}
\references{
Akcakaya, H. R., Burgman, M. A. and Ginzburg L.R. 1999. \emph{Applied Population Ecology}. Sinauer.

Caswell, H. 2003. \emph{Matrix Population Models: Construction, Analysis, and Interpretation }. Sinauer. 
}
\author{
Marcelino de la Cruz Rot
}

\examples{

  data(coryphanthaA)
  coryphanthaA
  coryphanthaA <- as.tmatrix(coryphanthaA)
  summary(coryphanthaA)
  plot(coryphanthaA) 
  
}


\keyword{ algebra }
\keyword{ models }
