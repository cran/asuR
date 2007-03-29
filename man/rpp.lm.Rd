\name{rpp.lm}
\alias{rpp.lm}
\title{partial residual vs. each predictor plot}
\description{
Plots residuals of a linear model against each predictor.
}
\usage{
\method{rpp}{lm}(mymodel, id = c("all", "none"), ...)
}
\arguments{
  \item{mymodel}{an object of class \code{lm}, usually, a result of a
    call to the function \code{lm}.}
  \item{id}{a character string or numeric value; in which panel should
    it be possible to interactively identify points}
  \item{...}{further arguments}
}
\details{
The shape of the scatter can help to find a appropriate transformation
for the focal predictors.
}
\value{
  A plot and a vector with the row index of the identified values
  in the original data.
}
\author{\email{thomas.fabbro@unibas.ch}}
\seealso{\code{\link{inspect}}}
\examples{
##
}
\keyword{models}
