\name{crime_cluster}
\alias{crime_cluster}
\docType{data}
\title{
 Crime Cluster
}
\description{
This data set contains statistics in arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973. It is an example for using kmeans clustering to find groups in a data set. 
}
\usage{data("crime_cluster")}
\format{
  A data frame with 50 observations on the following 3 variables.
  \describe{
    \item{\code{Row Names}}{Each of the 50 states}
    \item{\code{Murder}}{Number of Murder arrests per 100,000 residents}
    \item{\code{Assault}}{Number of assault arrests per 100,000 residents}
    \item{\code{Rape}}{Number of rape arrests per 100,000 residents}
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
data(crime_cluster)
StatePlot(2, crime_cluster)
}
\keyword{datasets}
