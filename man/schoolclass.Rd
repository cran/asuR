\name{schoolclass}
\alias{schoolclass}
\docType{data}
\title{Schools and Classes}
\description{
A data set for studying nested random factors.
}
\usage{data(schoolclass)}
\format{
  A data frame with 300 observations on the following 4 variables.
  \describe{
    \item{\code{school}}{a factor with 300 levels}
    \item{\code{class}}{a factor with 300 levels}
    \item{\code{gender}}{a factor with levels \code{female} \code{male}}
    \item{\code{score}}{a numeric vector}
  }
}
\details{
A simulated data set with the parameters: variance among schools (100),
variance among classes (50), residual variance (10), factor gender:
level female (200), level male (100).
}
\source{
thomas.fabbro@unibas.ch
}
\examples{
data(schoolclass)
str(schoolclass)
#
#with library(lme4)
# (m0 <- lmer(y ~ gender + (1|school/class), data=schoolclass))
}
\keyword{datasets}
