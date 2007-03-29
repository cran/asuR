\name{lep}
\alias{lep}
\alias{linearizedResponse.linearPredictor}
\title{linearized response vs. linear predictor plot}
\description{
There is a separate help page for every generic
}
\usage{
lep(mymodel, id = c("all", "none"), ...)
}
\arguments{
  \item{mymodel}{an object of class \code{glm}, usually, a result of a
    call to the function \code{glm}.}
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
\seealso{\code{\link{lep.glm}}, \code{\link{inspect}}}
\examples{
##
}
\keyword{models}
