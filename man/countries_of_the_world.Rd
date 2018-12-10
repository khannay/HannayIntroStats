\name{countries_of_the_world}
\alias{countries_of_the_world}
\docType{data}
\title{
CIA Factbook data for countries of the world
}
\description{
Contains some geographical and demographic data for countries of the world.
}
\usage{data("countries_of_the_world")}
\format{
  A data frame with 227 observations on the following 20 variables.
  \describe{
    \item{\code{Country}}{a character vector giving the countru name}
    \item{\code{Region}}{a factor with levels \code{ASIA (EX. NEAR EAST)} \code{BALTICS} \code{C.W. OF IND. STATES} \code{EASTERN EUROPE} \code{LATIN AMER. & CARIB} \code{NEAR EAST} \code{NORTHERN AFRICA} \code{NORTHERN AMERICA} \code{OCEANIA} \code{SUB-SAHARAN AFRICA} \code{WESTERN EUROPE}}
    \item{\code{Population}}{a numeric vector}
    \item{\code{Area.sq..mi.}}{a numeric vector}
    \item{\code{Pop..Density.per.sq..mi.}}{a numeric vector}
    \item{\code{Coastline.coast.area.ratio}}{a numeric vector}
    \item{\code{Net.migration}}{a numeric vector}
    \item{\code{Infant.mortality.per.1000.births}}{a numeric vector}
    \item{\samp{GDP.$.per.capita}}{a numeric vector}
    \item{\samp{Literacy.\%}}{a numeric vector}
    \item{\code{Phones.per.1000}}{a numeric vector}
    \item{\samp{Arable.\%}}{a numeric vector}
    \item{\samp{Crops.\%}}{a numeric vector}
    \item{\samp{Other.\%}}{a numeric vector}
    \item{\code{Climate}}{a numeric vector}
    \item{\code{Birthrate}}{a numeric vector}
    \item{\code{Deathrate}}{a numeric vector}
    \item{\code{Agriculture}}{a numeric vector}
    \item{\code{Industry}}{a numeric vector}
    \item{\code{Service}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
All these data sets are made up of data from the US government. Generally they are free to use if you use the data in the US. If you are outside of the US, you may need to contact the US Govt to ask. Data from the World Factbook is public domain. The website says "The World Factbook is in the public domain and may be used freely by anyone at anytime without seeking permission."
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(countries_of_the_world)
## maybe str(countries_of_the_world) ; plot(countries_of_the_world) ...
}
\keyword{datasets}
