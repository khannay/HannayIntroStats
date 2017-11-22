\name{soccer_coach}
\alias{soccer_coach}
\docType{data}
\title{
Soccer Coach Data Set
}
\description{
This is a fake data set giving soccer coach measurments of players times before and after implemented a new training regime for the team. The key information here is that the same players were used for both experiments.
}
\usage{data("soccer_coach")}
\format{
  A data frame with 30 observations on the following 3 variables.
  \describe{
    \item{\code{Player}}{a numeric vector giving the individual player used in the study}
    \item{\code{before.time}}{numeric vector giving the 40 yard dash time before the training was conducted}
    \item{\code{after.time}}{a numeric vector giving the 40 yard dash time of that player after the training was conducted. }
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(soccer_coach)
boxplot(soccer_coach$before.time, soccer_coach$after.time)
}
\keyword{datasets}
