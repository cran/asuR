\name{rpp.glm}
\alias{rpp.glm}
\title{Partial Residual Plots}
\description{
Plots the partial residuals of a glm-model versus each predictor
}
\usage{
\method{rpp}{glm}(mymodel, id=c("all", "none"), ...)
}
\arguments{
  \item{mymodel}{an object of class \code{glm}, usually, a result of a
    call to the function \code{glm}.}
  \item{id}{a character string or numeric value; in which panel should
    it be possible to interactively identify points; not yet available}
  \item{...}{further arguments}
}
\details{
A partial residual plot allows to study the effect of a focal predictor
and taking the other predictors into account. This helps to find an
appropriate transformation for the focal predictor.

***expected pattern:\cr
linear scatter of points along the regression line

***Question:\cr
Is the relationship linear? otherwise:\cr
1. change the transformations of the predictor 
}
\value{
  A plot and a vector with the row index of the identified values
  in the original data.
}
\author{\email{thomas.fabbro@unibas.ch}}
\seealso{\code{\link{inspect}}}
\examples{
#
}
\keyword{models}

