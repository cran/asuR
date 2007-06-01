\name{nrp}
\alias{nrp}
\alias{NormalQuantiles.Residuals.CategoricalFixedEffects}
\title{Normal quantile quantile plot of residuals by levels of categorical fixed effects}
\description{
There is a separate help page for every generic.
}
\usage{
nrp(mymodel, id = c("all", "none"), ...)
}
\arguments{
  \item{mymodel}{an object of class \code{lmer}, usually, a result of a
    call to the function \code{lmer}.}
  \item{id}{a character string or numeric value; in which panel should
    it be possible to interactively identify points}
  \item{...}{further arguments}
}
\details{
 There is a separate help page for every generic.
}
\value{
A plot and an invisible vector of row indices (in the original data) of identified values.
}
\author{\email{thomas.fabbro@unibas.ch}}
\seealso{\code{\link{nrp.lmer}}, \code{\link{inspect}}}
\examples{
##
}
\keyword{models}
