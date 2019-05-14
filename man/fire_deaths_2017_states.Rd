\name{fire_deaths_2017_states}
\alias{fire_deaths_2017_states}
\docType{data}
\title{
Accidental Fire Deaths in US 2017
}
\description{
This data set gives the number and rate per 100k residents of accidental fire deaths in all states in the united states in 2017
}
\usage{data("fire_deaths_2017_states")}
\format{
  A data frame with 51 observations on the following 4 variables.
  \describe{
    \item{\code{stateName}}{a character vector giving the state name}
    \item{\code{crudeNumber}}{a numeric vector giving the raw number of deaths}
    \item{\code{RatePerHundredThousand}}{a numeric vector giving the rate per 100k residents}
    \item{\code{population}}{a numeric vector giving the population of the state}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
CDC Data
https://wisqars-viz.cdc.gov:8006/
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(fire_deaths_2017_states)
}
\keyword{datasets}
