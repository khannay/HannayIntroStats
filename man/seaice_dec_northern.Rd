\name{seaice_dec_northern}
\alias{seaice_dec_northern}
\docType{data}
\title{
Sea Ice Data Set
}
\description{
This data set contains sea ice data for the northern hemisphere (goddard site) from 1978-2017. All measurements are taken during december of that year.
}
\usage{data("seaice_dec_northern")}
\format{
  A data frame with 40 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{a numeric vector giving the year of the measurement}
    \item{\code{extent}}{a numeric vector giving the extent of the ice}
    \item{\code{area}}{a numeric vector giving the area of the ice}
  }
}
\details{
The Sea Ice Index provides a quick look at Arctic- and Antarctic-wide changes in sea ice. The data are produced in a consistent way that makes the Index time-series appropriate for use when looking at long-term trends in sea ice cover. Both monthly and daily products are available. However, monthly products are better to use for long-term trend analysis because errors in the daily product tend to be averaged out in the monthly product and because day-to-day variations are often the result of short-term weather.
}
\source{
Data is downloaded from the national snow and ice data center.
}
\references{
ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/monthly/data/
}
\examples{
data(seaice_dec_northern)
}
\keyword{datasets}
