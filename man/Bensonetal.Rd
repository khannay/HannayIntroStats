\name{Bensonetal}
\alias{Bensonetal}
\docType{data}
\title{
Wolf Pack Data Set
}
\description{
Ungulate predation data collected from packs of canids in Ontario, Canada. Data include estimates of kill rates and biomass consumed and all covariates used in our analyses including genetic ancestry, pack size, snow depth, and prey availability. Also included are home range, territory sizes, days monitored, and study area.
}
\usage{data("Bensonetal")}
\format{
  A data frame with 23 observations on the following 29 variables.
  \describe{
    \item{\code{Pack}}{a factor with levels \code{Achray} \code{Axe} \code{BC} \code{BH} \code{GCL} \code{HS} \code{Jocko} \code{Laf} \code{Louis} \code{MCK} \code{NY} \code{Pine} \code{Potter} \code{Radiant} \code{Rain} \code{Ripple} \code{Skeleton} \code{Steve} \code{Storm} \code{Sunday} \code{Turtle} \code{Wier} \code{Wolf}}
    \item{\code{Year}}{a numeric vector, winter of 2006 (05-06), 2007 (06-07), 2010 (09-10)}
    \item{\code{Days}}{a numeric vector, giving Days = number of days predation was monitored}
    \item{\code{SA}}{a factor with levels \code{APP} \code{WMU49} study area (Algonquin Provinical Park [APP] or Wildlife Management Unit 49 [WMU49])}
    \item{\code{Size}}{a numeric vector, size of pack, i.e. number of canids}
    \item{\code{HR}}{a numeric vector, HR = home range size in km2}
    \item{\code{Terr}}{a numeric vector, territory size in km2}
    \item{\code{X.Coyote}}{a numeric vector, X.Coyote = admixture proportion for eastern coyote ancestry}
    \item{\code{SnowDepth}}{a numeric vector, SNOD = snow depth}
    \item{\code{MooseDen}}{a numeric vector, MooseDen = moose density [#/km2]}
    \item{\code{DeerHab}}{a numeric vector, DeerHab = proportion of home range comprising deer wintering habitat}
    \item{\code{DomAncest}}{a factor with levels \code{C} \code{M} \code{W}, DomAncest = C (coyote), W (wolf), M (mixed)}
    \item{\code{DKR}}{a numeric vector, DKR = deer kill rate, #/pack/day}
    \item{\code{MKR}}{a numeric vector, MKR = moose kill rate, #/pack/day }
    \item{\code{DKRC}}{a numeric vector, DKRC = per capita deer kill rate, deer/canid/day}
    \item{\code{MKRC}}{a numeric vector, MKRC = per capita moose kill rate, moose/canid/day}
    \item{\code{TKGC}}{a numeric vector, TKGC = per capita total ungulate kill rate, kg/canid/day}
    \item{\code{Deer}}{a numeric vector, Deer = kg deer acquired from kills}
    \item{\code{Moose}}{a numeric vector, Moose = kg moose acquired from kills}
    \item{\code{Total}}{a numeric vector, Total = kg total ungulate meat obtained from kills}
    \item{\code{DKGC}}{a numeric vector, DKGC = total deer meat per canid per day}
    \item{\code{MKGC}}{a numeric vector, MKGC = total moose meat per canid per day}
    \item{\code{TKGC_1}}{a numeric vector, TKGC = total ungulate meat per canid per day}
    \item{\code{DKRA}}{a numeric vector, DKRA = deer kill rate per unit area, deer/pack/day/km2}
    \item{\code{MKRA}}{a numeric vector, MKRA = moose kill rate per unit area, moose/pack/day/km2}
    \item{\code{KGCanid}}{a numeric vector, KGCanid = estimated kg of canids in pack}
    \item{\code{KGKGD}}{a numeric vector, KGKGD = kg deer/kg canid/day}
    \item{\code{KGKGM}}{a numeric vector, KGKGM = kg moose/kg canid/day}
    \item{\code{KGKGT}}{a numeric vector, KGKGT = kg ungulate/kgcanid/day}
  }
}
\details{

}
\source{
\link{http://datadryad.org/resource/doi:10.5061/dryad.3jd1g}
}
\references{
Benson JF, Loveless KM, Rutledge LY, Patterson BR (2017) Ungulate predation and ecological roles of wolves and coyotes in eastern North America. Ecological Applications 27(3): 718-733.

Benson JF, Loveless KM, Rutledge LY, Patterson BR (2017) Data from: Ungulate predation and ecological roles of wolves and coyotes in eastern North America. Dryad Digital Repository.
}
\examples{
data(Bensonetal)
}
\keyword{datasets}
