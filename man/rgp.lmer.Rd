\name{rgp.lmer}
\alias{rgp.lmer}
\title{Box- or dotplot of residuals by groups for each random factor}
\description{
A Boxplot (or if less than ten observations a dotplot) of the residuals
at every level of each random factor. THE IDENTIFICATION IS NOT YET IMPLEMENTED!
}
\usage{
\method{rgp}{lmer}(mymodel, id = c("all", "none"), ...)
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
  A) Are the residuals centered at zero? otherwise:\cr
  - 1. 
  B) is the variance constant? otherwise:\cr
  - a) if the variance changes among levels of a fixed factor  \cr
  - - 1) use a heteroscedastic model (only in function lme from package
  nlme)
  - b) 
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

