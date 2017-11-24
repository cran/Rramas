\name{projectn}
\alias{projectn}
\alias{project1}
\alias{estambi}
\alias{estdemo}
\alias{plot.rmas}
\alias{plot.summary.rmas}
\alias{summary.rmas}
\title{
Demographic Projections
}
\description{

Make deterministic and stochastic demographic projections according to a transition matrix.
}
\usage{
projectn(v0, mat, matsd = NULL, estamb = FALSE, estdem = FALSE,
         equalsign = TRUE,stmat=NULL, fecundity1=TRUE,
        nrep = 1, time = 10, management=NULL, round=TRUE)
project1(v0, mat, matsd=NULL, estamb=FALSE, estdem=FALSE,
         equalsign=TRUE, stmat=NULL, fecundity1=TRUE)
estambi(mat, matsd, equalsign)
estdemo(v0,mat,stmat=NULL, fecundity1=TRUE)
\method{plot}{rmas}(x, sum = TRUE, mean=FALSE, type="l", harvest=FALSE, ...)
\method{summary}{rmas}(object, stage=NULL, harvest=FALSE,...)
\method{plot}{summary.rmas}(x, ylim=NULL, col=NULL, xlab=NULL, ylab=NULL, main=NULL,...)
% \method{print}{summary.tmatrix}(x, ...)
}

\arguments{
  \item{v0}{ Vector with the initial abundance of each stage. }
  \item{mat}{ Transition matrix.}
  \item{matsd}{Matrix with the standard deviation of the probabilities in \code{mat}.}
  \item{estamb}{ Logical. Should environmental stochasticity be considered to projet the dynamics of the population?}
  \item{estdem}{ Logical. Should demographic stochasticity be employed to project the dynamics of the population?}
  \item{equalsign}{ Logical. Should the environmental deviations have all the same sign and magnitude? See details section.}
  \item{stmat}{Matrix indicating for each transition probability in \code{mat} which part (i.e. which proportion) should be considered resulting from 
               fecundity (and the rest will be considered resulting from survival). See details.  }
  \item{fecundity1}{Logical. Should the first row of \code{mat} be considered exclusively as fecundities? See details}
  \item{nrep}{ Number of replications  to evaluate the effects of stochasticity.}
  \item{time}{ length of the demographic trajectory}
  \item{management}{Vector (or matrix) of management actions to be applied each time step.    }  
  \item{round}{ Logical. Should the projections be rounded to the next integer each time step (i.e. consider finite indiviuals)?}  
  \item{object}{An object of class \code{rmas}, i.e. resulting from \code{projectn}.}
  \item{x}{An object resulting from \code{projectn} or \code{summary.rmas}.}
\item{stage}{Print only the trajectory of the stage called ...}
\item{harvest}{ Logical. Should the harvest history be summarized or ploted instead of the population one? }  
\item{sum}{Logical. If \code{TRUE}, print the trajectory of the whole population. If \code{FALSE}, print the individual trajectory of all stages. }
\item{mean}{Logical. If \code{TRUE}, print the mean trajectory of all replications. If \code{FALSE}, print all the replicated trajectories.}
\item{type}{Type of plot to represent the trajectories. By default, a line.  }
\item{ylim}{Vector with max and min values of the y (abundances) axis.  }
\item{col}{Color or vector of colors to draw the trajectories.}
\item{xlab}{Label for the x-axis.}
\item{ylab}{Label for the y-axis.}
\item{main}{Text to appear as title.}
\item{\dots}{Other parameters passed to plot and other methods.}

}
\details{
The function \code{projectn} makes demographic proyections by repetedly calling (as long as the time argument) to \code{project1}. If no environmental or demographic stochasticity is required, \code{project1} will multiply the transition matrix \code{mat} by the vector of stage abundances. 

If demographic stochasticity is required, \code{project1} will call to \code{estdemo} function, that for each time will assign abundances to each stage based in sampling from both a binomial and a poisson distribution. Sampling from \link{rbinom} with probability \code{mat[i,j]} will assign  "survival" chances to each individual accounted for in the abundance vector, i.e. it would allow some of them to remain in the same stages (for transitions \code{mat[i,i]}) or to pass to another stage (for transitions \code{mat[i,j]}). Sampling from \link{rpois} with mean \code{mat[i,j]} will assign to each individual of \code{stage[j]} a random number of offspring of type \code{stage[i]}. 

In the current implementation there are 3 options to generate demographic stochasticity. By deafult ( \code{stmat=NULL} and \code{fecundity1=TRUE}) probabilities in the first row of the transition matrix (i.e \code{mat[1,j]}) are assumed to represent \bold{only} fecundities, i.e., they would not account for "survival" transitions from stage[j] to stage[1], but only for newborns. This means that these probabilities will only be used as the mean for sampling from \link{rpois}. Transitions in rows others than first row will be assumed to represent "survival" transitions if its value is <=1 and accordingly will be employed to sample from \link{rbinom}. Transitions >1 will be assumed to represent fecundities and will be employed to sample from \link{rpois}.

If \code{stmat=NULL} and \code{fecundity1=FALSE} transition probabilities in all the rows of the matrix \code{mat} are treated in the same way, i.e. probabilities <=1 will be sampled from \link{rbinom} and probabilities >1 will be sampled from \link{rpois}.

If a \code{stmat} matrix (a matrix with values between 0 and 1) is provided, it will be used to divide transition probabilities \code{mat[i,j]} into fecundities (\code{mat[i,j] * stmat[i,j]}) and survival probabilities (\code{mat[i,j] - (mat[i,j] * stmat[i,j])}) and these matrices will be used to sample from \link{rbinom} and \link{rpois} respectively.

\emph{The current implementation of} \code{estdemo} \emph{assumes that reproduction takes place before "survival sorting"}, so even for individuals that wouldn't survive (according to its sampled binomial probability) offspring is computed (if approppriate) and accounted for.


If environmental stochasticity is required (i.e., a \code{matsd} matrix is provided), \code{project1} will call to \code{estambi} function, that for each time will change \code{mat[i,j]} probabilities sampling from \link{rnorm} with \code{mean= mat[i,j]} and \code{sd = matsd[i,j]}. If \code{equalsign=TRUE} the random changes in all cells of \code{mat} will have the same sign and the same magnitude (relative to each individual \code{matsd[i,j]}). If \code{equalsign=FALSE} every transition probability will change independently.

If both environmental and demographic stochasticity are required \code{project1} will call first to \code{estambi} and using the modified \code{mat} will call to \code{estdemo}.

If management is required, a vector or matrix of magement actions  should be provided. In the simplest case (i.e., a vector), each element in the vector will be interpreted as the management action that will be applied each time step to the corresponding stage. Positive and negative elements in the vector represent respectively the introduction or extraction of individuals from the corresponding stage. Elements whose absolute value is >= 1 will be interpreted as the introduction or extraction of exactly that number of individuals; absolute values < 1 will be interpreted as the introduction or extraction of that proportion of individuals from the existing individuals in the corresponding stage. If "management" is a matrix, each time step the management actions represented by each column of the matrix will be applied sequentially, from first to last.


}
\value{
\code{project1}, \code{estdemo} and \code{estambi} return a vector of stage abundances with the same length that \code{v0}.

\code{projectn} return an object of class \code{rmas}, basically a list with four elements: \code{vn}, with length = (\code{nrep}) where each of its elements represents a replicate simulation and consist of a matrix of dimensions \code{[lengtth(v0), time]} representing the abundance of each stage at each time. If the simulation included management actions, \code{harvest} will be a list of length (\code{nrep}) where each of its elements represents the trajectory of harvest in a replicate simulation and consist of a matrix of dimensions \code{[lengtth(v0), time]} representing the number of individuals extracted of each stage at each time. The other two elements, \code{mat} and \code{management},  are respectively the transition matrix and the managenet matrix employed in the simulations.
The \code{plot} method will draw the demographic trajectory of the population. By default (\code{sum = TRUE, mean=FALSE}) it will plot the abundance of the whole population (i.e. the the sum of abundances in each stage) vs. time. If nrep >1 it will plot together the trajectory of each replicated population. If ( \code{sum = TRUE, mean=TRUE}) it will plot the mean of all repplicated populations. If (\code{sum = FALSE, mean=TRUE}) it wil plot the abundance (or the mean abundance in all the replications) of each stage vs. time.

The \code{summary} and \code{plot.summary} methods will print a table and draw a plot respectively with the maximun, mean + 1 sd, mean, mean - 1 sd and minimum values of population abundance in all the simulations.

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
     coryphanthaA <- as.tmatrix(coryphanthaA)
  
     # run a deterministic simulation of 20 years from an initial state of
     # 100 small juveniles:
     v0 <- c(100,0,0)
     simu20 <- projectn(v0=v0, mat=coryphanthaA, time = 20)
     plot(simu20, sum=FALSE)
     summary(simu20)
  
     # run 100 simulations of 20 years with  demographic stochasticity:
     simu20.ds <- projectn(v0=v0, mat=coryphanthaA, time = 20, estdem=TRUE, nrep=100)
     plot(simu20.ds)
     summary(simu20.ds)
     
     # run 100 simulations of 20 years with  demographic stochasticity but 
     # assuming that the first row of the transition matrix represent both
     # fecundity and survival, each with a 50% contribution 
     
     # first generate the stmat matrix:
     stmat <- (coryphanthaA >0)
     stmat <- stmat*c(0.5,0,0)
     stmat
     
     simu20.ds2 <- projectn(v0=v0, mat=coryphanthaA, time = 20, estdem=TRUE,
                             stmat=stmat, nrep=100)
     plot(simu20.ds2)
     summary(simu20.ds2)
          
  
     # run 100 simulations of 20 years with  both demographic and environmental
     # stochasticity:
     # first generate a sd matrix to describe environmental stochasticity:
     sdenv <- coryphanthaA/20 
     sdenv
     
     simu20.eds <- projectn(v0=v0, mat=coryphanthaA, matsd =sdenv,  time = 20,
                            estdem=TRUE,estamb=TRUE, nrep=100)
     plot(simu20.eds)
     summary(simu20.eds)
     
     # Example of management actions
     #    each time step, 10 individuals will be added to the first stage ,10 individuals  
     #    will be added to the second stage, and 50 percent of the individuals in the 
     #    third stage will be extracted
     
        man <- c(10, 10, -0.5)
        p1 <- projectn(v0 = c(100, 100,100), mat= coryphanthaA, management=man)
	
        # summarize and plot population trajectory
        summary(p1) 
	
	# summarizes and plots harvest history
	summary(p1, harvest=TRUE)  
           

}

\keyword{ algebra }
\keyword{ models }
