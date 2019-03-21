\name{UFO_Sightings}
\alias{UFO_Sightings}
\docType{data}
\title{
Worldwide UFO Sightings 1949-2013
}
\description{
This data set has UFO sightings for a long time frame. It reports various characteristics of the reported UFO as well as geographical information.
}
\usage{data("UFO_Sightings")}
\format{
  A data frame with 80332 observations on the following 12 variables.
  \describe{
    \item{\code{datetime}}{a date and time together for the sighting}
    \item{\code{city}}{a character vector}
    \item{\code{state}}{a factor with levels \code{ab} \code{ak} \code{al} \code{ar} \code{az} \code{bc} \code{ca} \code{co} \code{ct} \code{dc} \code{de} \code{fl} \code{ga} \code{hi} \code{ia} \code{id} \code{il} \code{in} \code{ks} \code{ky} \code{la} \code{ma} \code{mb} \code{md} \code{me} \code{mi} \code{mn} \code{mo} \code{ms} \code{mt} \code{nb} \code{nc} \code{nd} \code{ne} \code{nf} \code{nh} \code{nj} \code{nm} \code{ns} \code{nt} \code{nv} \code{ny} \code{oh} \code{ok} \code{on} \code{or} \code{pa} \code{pe} \code{pq} \code{pr} \code{qc} \code{ri} \code{sa} \code{sc} \code{sd} \code{sk} \code{tn} \code{tx} \code{ut} \code{va} \code{vt} \code{wa} \code{wi} \code{wv} \code{wy} \code{yk} \code{yt}}
    \item{\code{country}}{a factor with levels \code{au} \code{ca} \code{de} \code{gb} \code{us}}
    \item{\code{shape}}{a factor with levels \code{changed} \code{changing} \code{chevron} \code{cigar} \code{circle} \code{cone} \code{crescent} \code{cross} \code{cylinder} \code{delta} \code{diamond} \code{disk} \code{dome} \code{egg} \code{fireball} \code{flare} \code{flash} \code{formation} \code{hexagon} \code{light} \code{other} \code{oval} \code{pyramid} \code{rectangle} \code{round} \code{sphere} \code{teardrop} \code{triangle} \code{unknown}}
    \item{\samp{duration (seconds)}}{a numeric vector}
    \item{\code{latitude}}{a numeric vector}
    \item{\code{longitude}}{a numeric vector}
    \item{\code{Date.Posted}}{a Date}
    \item{\code{Year}}{a numeric vector}
    \item{\code{Month}}{a numeric vector}
    \item{\code{Week.Day}}{a factor with levels \code{Friday} \code{Monday} \code{Saturday} \code{Sunday} \code{Thursday} \code{Tuesday} \code{Wednesday}}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
This dataset was scraped, geolocated, and time standardized from NUFORC data by Sigmond Axel here.
Taken from https://www.kaggle.com/NUFORC/ufo-sightings
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(UFO_Sightings)
barplot(table(UFO_Sightings$Year), las=2)
}
\keyword{datasets}
