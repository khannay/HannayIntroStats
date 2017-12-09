\name{BirdCaptureData}
\alias{BirdCaptureData}
\docType{data}
\title{
Bird Banding Data
}
\description{
Bird Capture data from the Coyote Creek banding station, San Jose, California, 1987 to 2008.
}
\usage{data("BirdCaptureData")}
\format{
  A data frame with 14309 observations on the following 7 variables.
  \describe{
    \item{\code{BandNumber}}{a numeric vector giving the bandnumber for the bird captured. Each individual bird has a unique number.}
    \item{\code{SpeciesCode}}{a factor with levels \code{OCWA} \code{SWTH} \code{WEFL} \code{WIWA} \code{YWAR}}
    \item{\code{Age}}{a factor with levels \code{0} \code{1} \code{2} \code{5} \code{6}}
    \item{\code{Sex}}{a factor with levels \code{F} \code{M} \code{U}}
    \item{\code{Yr}}{a numeric vector giving the year of the capture}
    \item{\code{Season}}{a factor with levels \code{fall} \code{spring}}
    \item{\code{JulianDay}}{a numeric vector 1 to 365}
  }
}
\details{
capture data for songbirds at the Coyote Creek banding station, San Jose, California, 1987 to 2008.

BandNumber = unique band number for each bird, 4-digit prefix and 5-digit suffix

SpeciesCode = AOU species codes for the study species.
OCWA = Orange-crowned Warbler
SWTH = Swainson's Thrush,
WEFL = Pacific-slope Flycatcher (formerly Western Flycatcher
WIWA = Wilson's Warbler
YWAR = Yellow Warbler

Age-class = USGS age codes
0 = unknown age-class
1 = after-hatch-year (AHY) or adult
2 = hatch-year (HY) or immature
5 = second-year (SY) or yearling

Sex-class = sex
F = female
M = male
U = unknown

Yr = year of study
1987 to 2008

Season
fall = July to November (5 mos)
spring = February to June (5 mos)

JulianDay = day of year for capture
1 to 365

}
\source{
Barton, G.G. and B.K. Sandercock. Long-term changes in the seasonal timing of songbird migration on the Pacific flyway.  Condor, in review.
}
\references{
\url{http://datadryad.org/resource/doi:10.5061/dryad.t00nn}
Barton GG, Sandercock BK (2017) Data from: Long-term changes in the seasonal timing of landbird migration on the Pacific Flyway. Dryad Digital Repository.
}
\examples{
data(BirdCaptureData)
}
\keyword{datasets}
