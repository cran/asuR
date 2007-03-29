\name{inspect}
\alias{inspect}
\alias{inspect.lm}
\alias{irp}
\alias{ryp}
\alias{ilp}
\alias{prp}
\alias{ihp}
\alias{app}
\title{Plots to inspect fitted models}
\description{
This functions can be used to validate some assumptions of linear
regressions. The idea of this functions is to illustrate possibilities,
not to provide fully functional code. The functions work for
regression models with continous variables only.
}
\usage{
inspect(mymodel, which = c("select", "all"), id = c("all", "none"), ...)
irp(mymodel)
ryp(mymodel, id= c("none", "all"))
ilp(mymodel)
prp(mymodel)
}
\arguments{
  \item{mymodel}{an object of class \code{glm} or \code{lm}, usually, a result of a
    call to the function \code{glm} or \code{lm}}
  \item{which}{a character string; do you want to \code{select} the plots from
    a list of all possible diagnostics plots available or do you want
    \code{all} of them, one after the other}
  \item{id}{a character string or numeric value; in which panel should
    it be possible to interactively identify values}
  \item{...}{further arguments}
}
\details{The function inspect calls other functions (depending on their
  class) to inspect the model assumptions.
}
\value{
A list with identified values (row names), one slot for each inspection function.
}
\author{thomas.fabbro@unibas.ch}
\seealso{plot}
\examples{
 data(mytrees)
 model <- lm(log(Volume) ~ log(Girth) + log(Height),  data=mytrees)
## inspect(model)
}
\keyword{file}
