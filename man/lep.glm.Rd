\name{lep.glm}
\alias{lep.glm}
\title{linearized response vs. linear predictor (eta) plot}
\description{
A plot to check the link assumption in a generalized linear model.
}
\usage{
\method{lep}{glm}(mymodel, id = c("all", "none"), ...)
}
\arguments{
  \item{mymodel}{an object of class \code{glm}, usually, a result of a
    call to the function \code{glm}.}
  \item{id}{a character string or numeric value; in which panel should
    it be possible to interactively identify points}
  \item{...}{further arguments}
}
\details{
  after violations of simpler model assumptions have been eliminated.

  ***Question: \cr
  Is the relationship linear? otherwise \cr
  1. change the link function 
}
\value{
A plot and a vector with identified values (corresponding to the row
number in the original data).
}
\author{\email{thomas.fabbro@unibas.ch}}
\seealso{\code{\link{inspect}}}
\examples{
#
}
\keyword{models}
