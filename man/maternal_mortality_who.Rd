\name{maternal_mortality_who}
\alias{maternal_mortality_who}
\docType{data}
\title{
Maternal Mortality 1990-2015 World
}
\description{
This data set has estimates of the maternal mortality rates of 183 countries from 1990-2015 every five years. It also contains some data from the Human Freedom index in 2016.
}
\usage{data("maternal_mortality_who")}
\format{
  A data frame with 183 observations on the following 16 variables.
  \describe{
    \item{\samp{ISO codes}}{a character vector a shorthand for the countries}
    \item{\samp{Countries_Regions}}{a character vector giving the full country name}
    \item{\samp{region}}{a character vector the region of the country}
    \item{\samp{1990}}{a numeric vector giving the estimated MMR for that year}
    \item{\samp{1995}}{a numeric vector}
    \item{\samp{2000}}{a numeric vector}
    \item{\samp{2005}}{a numeric vector}
    \item{\samp{2010}}{a numeric vector}
     \item{\samp{2015}}{a numeric vector}
     \item{\samp{pfscore}}{a numeric vector giving the overall Human Freedom Index for the country}
    \item{\samp{pf_ss_women_fgm}}{a numeric vector}
    \item{\samp{pf_ss_women_inheritance_widows}}{a numeric vector}
    \item{\samp{pf_ss_women_inheritance_daughters}}{a numeric vector}
    \item{\samp{pf_ss_women_inheritance}}{a numeric vector}
    \item{\samp{pf_ss_women}}{a numeric vector}
  }
}
\details{
The maternal mortality ratio (MMR) listed for each country and year is defined as the number of maternal deaths during a given time period per 100,000 live births during the same time period. It depicts the risk of maternal death relative to the number of live births and essentially captures the risk of death in a single pregnancy or a single live birth.

The human freedom index data makes up the columns 10-15 in this data set. This was taken from the data set in 2016.
The Human Freedom Index measures economic freedoms such as the freedom to trade or to use sound money, and it captures the degree to which people are free to enjoy the major freedoms often referred to as civil liberties—freedom of speech, religion, association, and assembly— in the countries in the survey. In addition, it includes indicators on rule of law, crime and violence, freedom of movement, and legal discrimination against same-sex relationships. We also include nine variables pertaining to women-specific freedoms that are found in various categories of the index.

On a scale of 0 to 10, where 10 represents more freedom, the average human freedom rating for 162 countries in 2016 was 6.89.

}
\source{
World Health Organization
https://data.unicef.org/topic/maternal-health/maternal-mortality/

https://www.kaggle.com/gsutters/the-human-freedom-index

}
\references{
Ian Vásquez and Tanja Porčnik, The Human Freedom Index 2018: A Global Measurement of Personal, Civil, and Economic Freedom (Washington: Cato Institute, Fraser Institute, and the Friedrich Naumann Foundation for Freedom, 2018).
}
\examples{
data(maternal_mortality_who)
}
\keyword{datasets}
