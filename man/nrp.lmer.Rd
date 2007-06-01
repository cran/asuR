\name{nrp.lmer}
\alias{nrp.lmer}
\title{Normal quantile quantile plot of random  effects, for each factor}
\description{
A normal quantile quantile plot of the random effects.
}
\usage{
\method{nrp}{lmer}(mymodel, id = c("all", "none"), ...)
}
\arguments{
  \item{mymodel}{an object of class \code{lmer}, usually, a result of a
    call to the function \code{lmer}.}
  \item{id}{a character string; in which panel should
    it be possible to interactively identify points}
    \item{...}{further arguments}
}
\details{
  ***Questions: \cr
  A) Do the points lie on a line?
}
\value{
A plot% and for the user identified points an invisible vector of row indices (corresponding to the original data).
}
\author{\email{thomas.fabbro@unibas.ch}}
\seealso{\code{\link{inspect}}}
\examples{
##
}
\keyword{models}

