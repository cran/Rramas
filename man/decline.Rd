\name{decline}
\alias{decline}
\alias{explosion}
\alias{summary.rmas.risk}
\alias{plot.summary.rmas.risk}
\title{
Compute declination/explosion probabilities
}
\description{
Compute declination (or explosion) probabilities for a stage-structured population. From a vector of initial stage abundances and a transition matrix, \code{decline} and \code{explosion} compute respectively the probability that the population size falls below or surpasses some abundance thresholds during a given time interval.
}
\usage{
decline(rmas, bootsp = 1000)
explosion(rmas, bootsp = 1000)
\method{summary}{rmas.risk}(object, q = c(0.025, 0.975),...)
\method{plot}{summary.rmas.risk}(x, ylim = NULL, col = NULL, xlab = NULL, ylab = NULL, main = NULL, ...)
}

\arguments{
  \item{rmas}{An object of class \code{rmas},i.e., resulting from \link{projectn}.}
  \item{bootsp}{number of botstraped samples.}
  \item{object}{An object of class \code{rmas.rsik}, i.e. resulting from \link{decline}or \link{explosion}.}
  \item{x}{An object of class \code{summary.rmas.rsik}.}
  \item{q}{vector of quantiles to compute bootstraped confidence intervals.}
  \item{ylim}{Vector with max and min values of the y (abundances) axis.  }
  \item{col}{Color or vector of colors to draw the trajectories.}
  \item{xlab}{Label for the x-axis.}
  \item{ylab}{Label for the y-axis.}
  \item{main}{Text to appear as title.}
  \item{\dots}{Other parameters passed to plot and other methods.}
   
}
\details{
Both \code{decline} and \code{explosion} require that some stochastic simulations for a given time interval had been previously constructed (using \link{projectn}). Using those simulations \code{decline} computes the probability of falling bellow some population threshold (and \code{explosion} the probability of surpassing it) as the ratio between the number of times that these threshold has been attained and the number of replications. The set of abundances in each time interval (in all the simulations) are bootstraped (i.e. sampled with replacement) to build a confidence interval.
}
\value{
Both \code{decline} and \code{explosion} return an object of class \code{rmas.rsik}, basically a list with the following elements
\item{cf.obs}{a \code{data.frame} with the evaluated thresholds and their probabilities.}
\item{cf.boot}{a list of data.frames similar to \code{cf.obs} for each bootstraped sample.} 
\item{abminbot}{a list of bootstraped minimum (or maximum for \code{explosion}) abundances for each replica in the rmas object.}
\item{main}{Text to appear as title when plotting the summary.}
The methods \code{summary} and \code{plot.summary} summarize the results and print and plot probabilities and bootstraped confidence interval of attainning a particular population threshold.
}
\references{
Akcakaya, H. R., Burgman, M. A. and Ginzburg L.R. 1999. \emph{Applied Population Ecology}. Sinauer.
Caswell, H. 2003. \emph{Matrix Population Models: Construction, Analysis, and Interpretation }. Sinauer. 
}

\author{
Marcelino de la Cruz \email{marcelino.delacruz@upm.es}
}

\examples{
  \dontrun{
     data(coryphanthaA)
     coryphanthaA <- as.tmatrix(coryphanthaA)
     #initial abundances:
     v0 <- c(100,0,0)
       
     # run 1000 simulations of 20 years with  demographic stochasticity:
     simu20.ds <- projectn(v0=v0, mat=coryphanthaA, time = 20, estdem=TRUE, nrep=1000)
     
     # compute declination probabilities
     simu20.ds.dec <- decline(simu20.ds)
     
     summary(simu20.ds.dec)
     
     }
}


\keyword{ models }

