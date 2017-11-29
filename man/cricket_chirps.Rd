\name{cricket_chirps}
\alias{cricket_chirps}
\docType{data}
\title{
Cricket Chirps versus Temperature
}
\description{
This data set provides a good example of simple linear regression between two variables. The striped ground cricket chirps more quickly for higher temperatures.
}
\usage{data("cricket_chirps")}
\format{
  A data frame with 15 observations on the following 2 variables.
  \describe{
    \item{\code{chirps.sec}}{a numeric vector giving chirps/sec for the striped ground cricket}
    \item{\code{temp.fahrenheit}}{a numeric vector giving temperature in degrees Fahrenheit}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
The Song of Insects by Dr.G.W. Pierce, Harvard College Press
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(cricket_chirps)
plot(cricket_chirps$temp.fahrenheit, cricket_chirps$chirps.sec)
}
\keyword{datasets}
