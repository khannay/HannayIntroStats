\name{Homing_Pigeons}
\alias{Homing_Pigeons}
\docType{data}
\title{
Homing Pigeon Decision Making Data
}
\description{
This data set measures group decision making in a group (flock). They measured the accuracy and time to make a decesion for homing pigeon flocks of different sizes.
}
\usage{data("Homing_Pigeons")}
\format{
  A data frame with 39 observations on the following 4 variables.
  \describe{
    \item{\code{Site}}{a numeric vector giving the site the data was collected at}
    \item{\code{Flock.size}}{a numeric vector giving the number of pigeons in the flock}
    \item{\code{Homing.direction.degrees}}{a numeric vector giving the Homing direction as the difference between the vanishing bearing and the correct direction.}
    \item{\code{Decision.making.time.sec}}{a numeric vector giving the seconds equired for the flock to decide on a direction}
  }
}
\details{
Abstract from the publication:
Social animals routinely are challenged to make consensus decisions about movement directions and routes. However, the underlying mechanisms facilitating such decision-making processes are still poorly known. A prominent question is how group members participate in group decisions. We addressed this question by examining how flocks of homing pigeons (Columba livia) decide their homing direction. We released newly formed flocks varying in size and determined the time taken to choose a homing direction (decision-making period) and the accuracy of that choice. We found that the decision-making period increases exponentially with flock size, which is consistent with a participatory decision-making process. We additionally found that there is no effect of flock size on the accuracy of the decisions made, which does not match with current theory for democratic choices of flight directions. Our combined results are better explained by a participatory choice of leaders that subsequently undertake the flock directional decisions. However, this decision-making model would only entirely fit with our results if leaders were chosen based on traits other than their navigational experience. Our study provides rare empirical evidence elucidating decision-making processes in freely moving groups of animals.
}
\source{
Santos CD, Przybyzin S, Wikelski M, Dechmann DKN (2016) Collective decision-making in homing pigeons: Larger flocks take longer to decide but do not make better decisions. PLOS ONE 11(2): e0147497. https://doi.org/10.1371/journal.pone.0147497

Santos CD, Przybyzin S, Wikelski M, Dechmann DKN (2016) Data from: Collective decision-making in homing pigeons: Larger flocks take longer to decide but do not make better decisions. Dryad Digital Repository. https://doi.org/10.5061/dryad.k15v4
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Homing_Pigeons)
boxplot(Homing_Pigeons$Decision.making.time.sec~Homing_Pigeons$Flock.size, xlab='Flock size', ylab='Decision Making Time', col='coral')
}
\keyword{datasets}
