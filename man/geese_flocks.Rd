\name{geese_flocks}
\alias{geese_flocks}
\docType{data}
\title{
Geese Flock Sizes in Norway
}
\description{
Pink-footed goose abatement using scaring tactics in Norway.
}
\usage{data("geese_flocks")}
\format{
  A data frame with 840 observations on the following 5 variables.
  \describe{
    \item{\code{flock.size}}{a numeric vector giving the estimated flock size on that day. Taken either in the morning or evening}
    \item{\code{flock.prev.day.size}}{a numeric vector giving the size of the flock the previous day}
    \item{\code{geese.binary}}{a numeric vector, 1 or 0 for wether any geese were present}
    \item{\code{geese.prev.day.binary}}{a numeric ector 1 or 0 whether geese where seen there the day before}
    \item{\code{scare.dose}}{a numeric vector, giving the number of times the geese were scared away that day}
  }
}
\details{
Increasing population sizes of geese are the cause of numerous agricultural conflicts in many regions of the Northern Hemisphere. Scaring is often used as a tool to chase geese away from fields, either as a means to protect vulnerable crops or as part of goose management schemes to drive geese to accommodation areas. Geese are quick to habituate to stationary scaring devices; hence, active scaring by humans is often employed. However, it remains undocumented how much effort is required for active scaring to be effective. We explored the relationship between intensity of active human scaring on field use and behaviour by geese. Using an experimental framework, we applied four different scaring doses per day (geese were scared either 2, 5, 7 or 10 times per day), to random pastures in a pink-footed goose spring staging area in mid-Norway, and recorded goose flock sizes, fleeing response distances, and average weekly goose densities assessed by dropping densities. In addition, we counted droppings in fields without scaring. We used mixed models to test for changes in the effects of different scaring doses over time and compared observed with predicted dropping levels. Cumulative dropping densities increased at different rates depending on the scaring dose. Scaring dosage did not affect flock size and fleeing response distance during the study period, but both flock sizes and fleeing response distances changed with time. Scaring dose 2 did not show any decrease in relative goose use compared to the fields without scaring, whereas doses 5, 7 and 10 all showed 74–78\% fewer droppings by the end of the spring staging period, indicating a possible threshold between dose 2 and 5. The largest effect of scaring appeared during the first week of scaring. Synthesis and applications. This study is the first to show a dose–response relationship between active scaring and field use of flocking geese. For individual farmers, the study provides guidance on the level of scaring effort needed to be cost-effective. If implemented as part of a management scheme with subsidy/accommodation areas in combination with systematic and persistent scaring, it can be used as a tool to keep geese away from areas where they are not wanted, thereby assisting in the alleviation of goose–agriculture conflicts. The approach in this study can be adapted and used in a wider range of wildlife interactions with human economic interests.
}
\source{
Simonsen CE, Madsen J, Tombre IM, Nabe-Nielsen J (2015) Is it worthwhile scaring geese to alleviate damage to crops? – an experimental study. Journal of Applied Ecology 53: 916-924. https://doi.org/10.1111/1365-2664.12604
}
\references{
Simonsen CE, Madsen J, Tombre IM, Nabe-Nielsen J (2016) Data from: Is it worthwhile scaring geese to alleviate damage to crops? – an experimental study. Dryad Digital Repository. https://doi.org/10.5061/dryad.62b84
}
\examples{
data(geese_flocks)
}
\keyword{datasets}
