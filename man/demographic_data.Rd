\name{demographic_data}
\alias{demographic_data}
\docType{data}
\title{
United States Demographic Data and Real Estate Investment Risks
}
\description{
This data set gives summary statistics for over 11,000 US cities. This came orginally from a real estate investment consulting company.
}
\usage{data("demographic_data")}
\format{
  A data frame with 11267 observations on the following 8 variables.
  \describe{
    \item{\code{state}}{a character vector giving the US state of the city}
    \item{\code{city}}{a character vector giving the city name}
    \item{\code{pop}}{a numeric vector giving the population of the city}
    \item{\code{household_income_mean}}{a numeric vector giving the estimated mean household income for the city}
    \item{\code{household_income_median}}{a numeric vector giving the estimated median income for the city}
    \item{\code{debt.proportion}}{a numeric vector giving the proportion of the population which has some form of debt}
    \item{\code{hs.degree.proportion}}{a numeric vector giving the proportion of the city population which graduated from high school}
    \item{\code{married.proportion}}{a numeric vector giving the proportion of the city population who are married}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Collected from numerous US Census Bureau sources
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(demographic_data)
}
\keyword{datasets}
