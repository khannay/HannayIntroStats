\name{Distel_Turtle_Data}
\alias{Distel_Turtle_Data}
\docType{data}
\title{
Dr. Distel Turtle Data Set
}
\description{
This data set was generously provided for the class by Dr. Distel, Schreiner University, Biology Department.
Original question: Do turtles benefit from living on private vs. public land?
Note this data set has NA values.
}
\usage{data("Distel_Turtle_Data")}
\format{
  A data frame with 290 observations on the following 12 variables.
  \describe{
    \item{\code{Property}}{a factor with levels \code{BGAD} and \code{CKWMA} see below for a description}
    \item{\code{Species}}{a factor with levels \code{Chelydra serpentina} and \code{Trachemys scripta} giving the species of the turtles. }
    \item{\code{Sex}}{Sex of the Turtles, a factor with levels \code{F} \code{F?} \code{ind} \code{J} \code{juv} \code{M} \code{M?} \code{NM}}
    \item{\code{Pond}}{Pond of Capture, a factor with levels \code{1} \code{10} \code{2} \code{3} \code{4} \code{5} \code{6} \code{8} \code{9} \code{B 100} \code{B800} \code{Bcattle} \code{BP1} \code{BP2} \code{Buck} \code{Gem} \code{Golf} \code{Henron} \code{Knorth} \code{Oxbow} \code{VE} \code{VW}}
    \item{\code{Carapace.Length}}{a numeric vector}
    \item{\code{Carapace.Width}}{a numeric vector}
    \item{\code{Plastron.Length}}{a numeric vector}
    \item{\code{Plastron.Anterior.Width}}{a numeric vector}
    \item{\code{Plastron.Posterior.Width}}{a numeric vector}
    \item{\code{Shell.Depth}}{a numeric vector}
    \item{\code{Tail.Length}}{a numeric vector}
    \item{\code{Mass}}{a numeric vector giving the Mass of the turtles in grams}
  }
}
\details{
Two properties: Blue Grass Army Depot (BGAD) is restricted access, but some fishing is still allowed;   Central Kentucky Wildlife Mgt Area (CKWMA) is public and open to hunting and fishing. Each property had multiple ponds in which both turtle species were captured. Four different trap types were used to account for differences in behavior between species, sex, or life stage. Generally, larger turtles have either lived longer or had better nutrients (or both) that smaller turtles of the same sex of the same species.
}
\source{
This data should not be posted on any public data set. Dr. Distel retains all rights to any publication for this data.
}
\references{
Wikipedia pages for the turtle species in the study:

\link{https://en.wikipedia.org/wiki/Common_snapping_turtle}

\link{https://en.wikipedia.org/wiki/Pond_slider}
}
\examples{
data(Distel_Turtle_Data)
boxplot(Distel_Turtle_Data$Mass~Distel_Turtle_Data$Species)
}
\keyword{datasets}
