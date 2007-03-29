\name{escapedDeparse}
\alias{escapedDeparse}
\title{escapedDeparse}
\description{
creates a string out of character vector where the individual elemets
are within escaped quotes
}
\usage{
escapedDeparse(vec.i)
}
\arguments{
  \item{vec.i}{a vetor whose elements are character}
}
\details{
This function is useful if you want to give a vector with character arguments to the
paste function and then parse the result. See e.g. inspect
}
\value{
  a character string
}
\references{ ~put references to the literature/web site here ~ }
\author{\email{thomas.fabbro@unibas.ch}}
\seealso{\code{\link{inspect}}}
\examples{
my.vec <- c("a", "b")
escapedDeparse(my.vec)
## returns
## [1] "c(\"a\",\"b\")"
}
\keyword{internal}