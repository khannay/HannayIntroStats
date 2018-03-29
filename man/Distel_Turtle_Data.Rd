\name{Distel_Turtle_Data}
\alias{Distel_Turtle_Data}
\docType{data}
\title{
Professor Distel Turtle Data
}
\description{
This data set contains measurement data collected by Dr. Chris Distel of Schreiner University on Turtles.
}
\usage{data("Distel_Turtle_Data")}
\format{
  A data frame with 290 observations on the following 12 variables.
  \describe{
    \item{\code{Property}}{a factor with levels \code{BGAD} \code{CKWMA}}
    \item{\code{Species}}{a factor with levels \code{Chelydra serpentina} \code{Trachemys scripta} giving the species of the turtles}
    \item{\code{Sex}}{a factor with levels \code{F} \code{F?} \code{ind} \code{J} \code{juv} \code{M} \code{M?} \code{NM}}
    \item{\code{Pond}}{a factor with levels \code{1} \code{10} \code{2} \code{3} \code{4} \code{5} \code{6} \code{8} \code{9} \code{B 100} \code{B800} \code{Bcattle} \code{BP1} \code{BP2} \code{Buck} \code{Gem} \code{Golf} \code{Henron} \code{Knorth} \code{Oxbow} \code{VE} \code{VW}}
    \item{\code{Carapace.Length}}{a numeric vector}
    \item{\code{Carapace.Width}}{a numeric vector}
    \item{\code{Plastron.Length}}{a numeric vector}
    \item{\code{Plastron.Anterior.Width}}{a numeric vector}
    \item{\code{Plastron.Posterior.Width}}{a numeric vector}
    \item{\code{Shell.Depth}}{a numeric vector}
    \item{\code{Tail.Length}}{a numeric vector}
    \item{\code{Mass}}{a numeric vector}
  }
}
\details{
Two properties: Blue Grass Army Depot (BGAD) is restricted access, but some fishing is still allowed;   Central Kentucky Wildlife Mgt Area (CKWMA) is public and open to hunting and fishing.
}
\source{
Dr. Distel retains all rights to this data for use in publications. This is to be used as an example and project data set for MATH 2330 at Schreiner University only.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Distel_Turtle_Data)
}
\keyword{datasets}
