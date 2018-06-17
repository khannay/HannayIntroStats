\name{flightNYC}
\alias{flightNYC}
\docType{data}
\title{
NYC Flight Data for 2013
}
\description{
On-time data for a sample of 5000 flights that departed NYC (i.e. JFK, LGA or EWR) in 2013. Delays are measured in minutes.
}
\usage{data("flightNYC")}
\format{
  A data frame with 5000 observations on the following 14 variables.
  \describe{
    \item{\code{month}}{a numeric vector giving the month of the flight}
    \item{\code{day}}{a numeric vector giving the day of the flight}
    \item{\code{dep_time}}{a numeric vector giving the actual departure time in local time}
    \item{\code{sched_dep_time}}{a numeric vector giving the scheduled departure time (local time)}
    \item{\code{dep_delay}}{a numeric vector giving the delay in the departure. If neagtive then left early}
    \item{\code{arr_time}}{a numeric vector giving actual arrival time in the local time}
    \item{\code{sched_arr_time}}{a numeric vector giving the scheduled arrival time in local time}
    \item{\code{arr_delay}}{a numeric vector giving the delay in the arrival of the flight}
    \item{\code{carrier}}{a factor giving the flight carrier shortcode}
    \item{\code{flight}}{a numeric vector giving the flight number}
    \item{\code{origin}}{a factor giving the airport code for the origin JFK, LGA or EWR}
    \item{\code{dest}}{a character vector giving the airport code for the destination}
    \item{\code{air_time}}{a numeric vector giving the time in the air for the flight}
    \item{\code{distance}}{a numeric vector giving the distance between the airport in miles}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
RITA, Bureau of transportation statistics, http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236
}
\references{
This is a pruned version of the dataset in the R Package nycflights13, this is a sample of 500 flights from the full set in the data set flightNYCFull
}
\examples{
data(flightNYC)
hist(flightNYC$dep_delay)
}
\keyword{datasets}
