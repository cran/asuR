\name{rfp.lmer}
\alias{rfp.lmer}
\title{Residuals vs. fitted valus for all categorical fixed effects}
\description{
A scatterplot of residuals vs. fitted values for all categorical fixed effects. THE IDENTIFICATION IS NOT YET IMPLEMENTED!
}
\usage{
\method{rfp}{lmer}(mymodel, id = c("all", "none"), ...)
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
  A) 
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

