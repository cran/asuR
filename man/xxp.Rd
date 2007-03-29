\name{xxp}
\alias{xxp}
\title{Plot all explanatory variables against each other}
\description{
A matrix with scatterplots and boxplots of all variables of a data set
against each other.
}
\usage{
xxp(mydata)
}
\arguments{
  \item{mydata}{a data frame}
}
\details{
Similar to the function \code{pairs}, but all variables are only plotted
once against eacht other. This is especially of advantage, if a data set
has many variables. Depending on the class of the variables different
plots are produced: scatterpots if both are numeric, boxplots if one is
numeric and the other is categorical
}
\value{
a plot
}
\author{\email{thomas.fabbro@unibas.ch}}
\keyword{file}
