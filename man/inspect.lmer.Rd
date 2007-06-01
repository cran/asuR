\name{inspect.lmer}
\alias{inspect.lmer}
\title{Plots to Inspect fitted Models}
\description{
This functions can be used to validate some assumptions of (generalized)
linear mixed effects models fitted using the \code{lmer} function from
package 'lme4'.
}
\usage{
\method{inspect}{lmer}(mymodel, which = c("select", "sequence", "all"), id = c("all", "none"), ...)
}
\arguments{
  \item{mymodel}{an object of class \code{lmer}, usually the result of a
    call to the function \code{lmer}}
  \item{which}{a character string; do you want to \code{select} the plots from
    a list of all possible diagnostics plots available or do you want
    \code{all} of them, one after the other}
  \item{id}{a character string or numeric value; in which panel should
    it be possible to interactively identify values}
  \item{...}{further arguments}  
}
\details{For lmer available:
  \tabular{lll}{
    \bold{description}          \tab     short &    \tab long function
    names \cr
           Normal quantile quantile plot of residuals by levles of
	   categorical fixed effects \tab        nrp \tab
	   NormalQuantiles.Residuals.CategoricalFixedEffects\cr
	   Box- or dotplot of residuals by levels, for each random
	   factor \tab rgp \tab residuals.by.groups\cr
	   Residuals vs. fitted values for all categorical fixed effects \tab rfp
	   \tab Residuals.Fitted.CategoricalFixedEffects\cr
	   Normal quantile quantile plot of random effects, for each
	   factor \tab nep \tab NormalQuantiles.RandomEffectsQuantiles\cr
  }
}
\value{
A list with identified values (row indices), one named slot (short name)
for each inspection function and an additional slot (called all) with
all values that were selected at lest in one plot.
}
\author{\email{thomas.fabbro@unibas.ch}}
\note{identification of points is not implemented yet}
\seealso{plot}
\examples{
##  data(wellplate)
##  model <- lmer(int ~ trt + (1|row) + (1|col), data=wellplate)
##  inspect(model)
}
\keyword{file}
