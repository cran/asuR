\name{inspect.glm}
\alias{inspect.glm}
\title{Plots to Inspect fitted Models}
\description{
This functions can be used to validate some assumptions of generalized linear
models.
}
\usage{
\method{inspect}{glm}(mymodel, which = c("select", "sequence", "all"), id = c("all", "none"), ...)
}
\arguments{
  \item{mymodel}{an object of class \code{glm}, usually the result of a
    call to the function \code{glm}}
  \item{which}{a character string; do you want to \code{select} the plots from
    a list of all possible diagnostics plots available or do you want
    \code{all} of them, one after the other}
  \item{id}{a character string or numeric value; in which panel should
    it be possible to interactively identify values}
  \item{...}{further arguments}  
}
\details{For glm available:
  \tabular{lll}{
    \bold{description}          \tab     short &    \tab long function names \cr
    deviance residuals vs. linear predictor            \tab dep \tab  devianceResidual.linearPredictor\cr
    partial residual vs. each predictor                \tab rpp \tab  partialResidual.eachPredictor \cr
    linearized response vs. linear predictor           \tab lep \tab  linearizedResponse.linearPredictor\cr
    half-normal quantiles vs. absolute stud. residuals \tab hnp \tab halfNormalQuantiles.absoluteStudentizedResiduals
  }
}
\value{
A list with identified values (row indices), one named slot (short name)
for each inspection function and an additional slot (called all) with
all values that were selected at lest in one plot.
}
\author{\email{thomas.fabbro@unibas.ch}}
\note{
  This functions help to inspect glm's with continuous predictors,
  sometimes they are anyway informative also for categorical predictors
  ...
}
\seealso{plot}
\examples{
##  data(gala)
##  model <- glm(Species ~ log(Area) + log(Elevation) + log(Nearest) + log(Scruz+0.1) + log(Adjacent), family=poisson(link=log), data=gala)
##  inspect(model)
}
\keyword{file}
