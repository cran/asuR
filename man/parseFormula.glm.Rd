\name{parseFormula.glm}
\alias{parseFormula.glm}
\title{providing names and data from model objects}
\description{
internal use only
}
\usage{
\method{parseFormula}{glm}(mymodel, ...)
}
\arguments{
  \item{mymodel}{ a fitted model object }
  \item{...}{further arguments}
}
\details{
}
\value{
  \item{response.var}{the name of the response variable}
  \item{response.term}{the name with transformation of the response variable}
  \item{predict.vars.numeric}{the name of the numeric predictor
    variables}
  \item{predict.terms.numeric}{the name with transformation of the
    numeric predictor variables}
  \item{index_coef.terms.numeric}{the index of the numeric terms in the
    vector returned by coef()}
  \item{my.data}{the data used for model fitting}
  \item{response.values}{the values of the response variable}
  \item{intercept.logical}{logical flag: is there an intercept}
}
\author{\email{thomas.fabbro@unibas.ch}}
\note{under construction}
\seealso{\code{\link{inspect}}}
\examples{
##
}
\keyword{models}