\name{dep.glm}
\alias{dep.glm}
\title{deviance residuals vs. linear predictor plot}
\description{
Plots the deviance residuals versus the linear predictor (eta).
}
\usage{
\method{dep}{glm}(mymodel, id = c("all", "none"), ...)
}
\arguments{
  \item{mymodel}{an object of class \code{glm}, usually, a result of a
    call to the function \code{glm}.}
  \item{id}{a character string; in which panel should
    it be possible to interactively identify points}
    \item{...}{further arguments}
}
\details{
  ***expected pattern:\cr
  random scatter in a horizontal band
  
  ***Questions: \cr
  A) is the relationship linear? otherwise:\cr
  1. change the choice of predictors \cr
  2. change the transformations of the predictor \cr
  3. change the link function (but there are only few choices...) \cr
  4. do not transform the response in glm since this would change the distribution 
  of the response (you would do this in a lm) \cr
  B) is the variance constant? otherwise:\cr
  1. change the variance function \cr
  2. use weights if you identify some features of the data that suggest a suitable 
  choice 
}
\value{
A plot and for the user identified points an invisible vector of row indices (corresponding to the original data).
}
\author{\email{thomas.fabbro@unibas.ch}}
\seealso{\code{\link{inspect}}}
\examples{
##
}
\keyword{models}

