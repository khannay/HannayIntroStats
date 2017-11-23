\name{alligator}
\alias{alligator}
\docType{data}
\title{
Alligator
}
\description{
This data is for a study in central Florida where 15 alligators were captured and two measurements were made on each of the alligators. The weight (in pounds) was recorded with the snout vent length (in inches – this is the distance between the back of the head to the end of the nose).
}
\usage{data("alligator")}
\format{
  A data frame with 15 observations on the following 2 variables.
  \describe{
    \item{\code{lnLength}}{a numeric vector giving the snout vent length}
    \item{\code{lnWeight}}{a numeric vector giving the weight of the alligators}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Mathematical Statistics with Applications by Mendenhall, Wackerly and Scheaffer (Fourth Edition – Duxbury 1990).
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(alligator)
lm(alligator$lnWeight~alligator$lnLength)
}
\keyword{datasets}
