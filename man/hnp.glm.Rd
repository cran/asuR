\name{hnp.glm}
\alias{hnp.glm}
\title{half normal quantiles vs. absolute studentized residuals plot}
\description{
In linear regression we inspect the normal quantile plot to check the
normality of residuals. For generalized linear models we do not
expect that the residuals are normally distributed. Nevertheless the
half-normal plot can help us to find outliers. Outliers can be detected
as points off the trend.
}
\usage{
\method{hnp}{glm}(mymodel, id= c("all", "none"), ...)
}
\arguments{
  \item{mymodel}{an object of class \code{glm}, usually, a result of a
    call to the function \code{glm}.}
  \item{id}{a character string or numeric value; in which panel should
    it be possible to interactively identify points}
  \item{...}{further arguments}
}
\details{
  ***expected pattern: \cr
  all points are scattered along a line

  ***question: 
  are there points of the trend? otherwise \cr
  1. Is the data point correct?

  (keep attention that this is not a quantile--quantile plot where you
  check for normality!)
}
\value{
A plot and a vector with identified values (corresponding to the row
nomber in the original data).
}
\author{\email{thomas.fabbro@unibas.ch}}
\seealso{\code{\link{inspect}}}
\examples{
#
}
\keyword{models}

