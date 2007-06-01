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
inspect(mymodel, which = c("select", "sequence", "all"), id = c("all", "none"), ...)
irp(mymodel)
ryp(mymodel, id= c("none", "all"))
ilp(mymodel)
prp(mymodel)
}
\arguments{
  \item{mymodel}{an object of class \code{glm} or \code{lm}, usually, a result of a
    call to the function \code{glm} or \code{lm}}
  \item{which}{a character string; one of the following:
    \description{
      \item{\code{select}}{
	select a diagnostic plot from a menu with all plots available
	(also repeatedly selecting the same plot is possible)
      }
      \item{\code{sequence}}{
	plot all available diagnostic plots in a sequence
	(with the possibility to skip a plot or quit)
      }
      \item{\code{all}}{
	plot all available diagnositc plots without user interaction
	(mainly useful for printing, see examples)
      }
    }
  }
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
\author{\email{thomas.fabbro@unibas.ch}}
\note{On some devices you are not allowed to resize the window before
  interactively selecting points}
\seealso{plot}
\examples{

 data(mytrees)
 model <- lm(log(Volume) ~ log(Girth) + log(Height),  data=mytrees)
## inspect(model)

### for printing
##pdf(~/temp/diagnostic.pdf)
##inspect(model)
##dev.off()
}
\keyword{file}
