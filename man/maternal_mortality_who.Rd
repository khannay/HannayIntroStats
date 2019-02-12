\name{maternal_mortality_who}
\alias{maternal_mortality_who}
\docType{data}
\title{
Maternal Mortality 1990-2015 World
}
\description{
This data set has estimates of the maternal mortality rates of 183 countries from 1990-2015 every five years.
}
\usage{data("maternal_mortality_who")}
\format{
  A data frame with 183 observations on the following 8 variables.
  \describe{
    \item{\samp{ISO3 codes}}{a character vector a shorthand for the countries}
    \item{\samp{Countries/Regions}}{a character vector giving the full country name}
    \item{\samp{1990}}{a numeric vector giving the estimated MMR for that year}
    \item{\samp{1995}}{a numeric vector}
    \item{\samp{2000}}{a numeric vector}
    \item{\samp{2005}}{a numeric vector}
    \item{\samp{2010}}{a numeric vector}
    \item{\samp{2015}}{a numeric vector}
  }
}
\details{
The maternal mortality ratio (MMR) listed for each country and year is defined as the number of maternal deaths during a given time period per 100,000 live births during the same time period. It depicts the risk of maternal death relative to the number of live births and essentially captures the risk of death in a single pregnancy or a single live birth.
}
\source{
World Health Organization
https://data.unicef.org/topic/maternal-health/maternal-mortality/
}
\references{

}
\examples{
data(maternal_mortality_who)
}
\keyword{datasets}
