\name{running}
\alias{running}
\docType{data}
\title{
Dr. Hannay Running Data 2018-2019
}
\description{
This data set contains a sampling of 52 runs recorded by Dr. Hannay's fitness watch. This is meant
to provide an example for how you can track your own data using R and applied statistics concepts.
}
\usage{data("running")}
\format{
  A data frame with 52 observations on the following 12 variables.
  \describe{
    \item{\code{Date}}{a Date giving the date of the run}
    \item{\code{Distance}}{a numeric vector giving the distance of the run in miles}
    \item{\code{Calories}}{a numeric vector giving the estimated calrories burned during the run}
    \item{\code{Time}}{a hour minutes second formated time. This is the total time of the run}
    \item{\code{Avg_HR}}{a numeric vector gives the average estimated HR during the run}
    \item{\code{Max_HR}}{a numeric vector gives the max heart rate estimated during the run}
    \item{\code{Avg_Run_Cadence}}{a numeric vector gives the cadence of the run. Steps per minute}
    \item{\code{Avg_Pace}}{a numeric vector giving the minutes per mile on average for the run}
    \item{\code{Best_Pace}}{a numeric vector giving the best recorded minutes per mile on the run}
    \item{\code{Elev_Gain}}{a numeric vector total gain in ft}
    \item{\code{Elev_Loss}}{a character vector total downhill in ft}
    \item{\code{Avg_Stride_Length}}{a numeric vector estimated average stride length for the run. In ft. }
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
data(running)
plot(running$Date, running$Best_Pace)
plot(running$Date, running$Avg_Pace)
}
\keyword{datasets}
