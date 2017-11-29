\name{bacon_data}
\alias{bacon_data}
\docType{data}
\title{
Bacon Data
}
\description{
This is a fake data set which gives an example of a data set with a small R^2 value, but still has utility in understanding health outcomes.
}
\usage{data("bacon_data")}
\format{
  A data frame with 100 observations on the following 2 variables.
  \describe{
    \item{\code{bacon.lbs}}{a numeric vector, lifetime bacon comsumption in pounds}
    \item{\code{life.expectancy}}{a numeric vector, giving the length of life for that fake subject}
  }
}
\details{
The idea here is that many factors influence life expectancy besides how much bacon you eat. However, bacon comsumption can still be have important effects on life expectancy.
}
\source{
Fake Data (no pigs were harmed in generating this data set)
}
\references{
None
}
\examples{
data(bacon_data)
## maybe str(bacon_data) ; plot(bacon_data) ...
}
\keyword{datasets}
