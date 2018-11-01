\name{vaccines_cdc}
\alias{vaccines_cdc}
\docType{data}
\title{
CDC Vaccine Data
}
\description{
Vaccination coverage for selected diseases among children aged 19–35 in the united states. Years range from 1998-2016 with some missing data entries.
}
\usage{data("vaccines_cdc")}
\format{
  A data frame with 19 observations on the following 12 variables.
  \describe{
    \item{\code{Year}}{a numeric vector giving the year of the data}
    \item{\samp{7.vaccine.series}}{a numeric vector}
    \item{\code{DTP.DT.DTaP}}{a numeric vector}
    \item{\code{Polio}}{a numeric vector}
    \item{\code{Measles.Mumps.Rubella}}{a numeric vector}
    \item{\code{Hib}}{a numeric vector}
    \item{\code{HepatitisA}}{a numeric vector}
    \item{\code{HepatitisB}}{a numeric vector}
    \item{\code{Varicella}}{a numeric vector}
    \item{\code{PCV}}{a numeric vector}
    \item{\code{Rotavirius}}{a numeric vector}
    \item{\code{Combined.7.vaccine.series}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
Taken from the cdc website
https://www.cdc.gov/nchs/hus/contents2017.htm#066
}
\examples{
data(vaccines_cdc)
plot(vaccines_cdc$Year, vaccines_cdc$Measles.Mumps.Rubella)
}
\keyword{datasets}
