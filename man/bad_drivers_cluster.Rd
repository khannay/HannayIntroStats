\name{bad_drivers_cluster}
\alias{bad_drivers_cluster}
\docType{data}
\title{
Which State Has The Worst Drivers?
}
\description{
This data comes from a five thirty eight story. The raw data behind the story "Dear Mona, Which State Has The Worst Drivers?" http://fivethirtyeight.com/datalab/which-state-has-the-worst-drivers/
}
\usage{data("bad_drivers")}
\format{
  A data frame with 51 observations on the following 7 variables. The row names of this data set give the states plus Washington D.C.
  \describe{
    \item{\code{num_drivers}}{Number of drivers involved in fatal collisions per billion miles}
    \item{\code{perc_speeding}}{Percentage of drivers involved in fatal collisions who were speeding}
    \item{\code{perc_alcohol}}{Percentage of drivers involved in fatal collisions who were alcohol-impaired}
    \item{\code{perc_not_distracted}}{Percentage of drivers involved in fatal collisions who were not distracted}
    \item{\code{perc_no_previous}}{Percentage of drivers involved in fatal collisions who had not been involved in any previous accidents}
    \item{\code{insurance_premiums}}{Car insurance premiums ($)}
    \item{\code{losses}}{Losses incurred by insurance companies for collisions per insured driver ($)}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
National Highway Traffic Safety Administration 2012, National Highway Traffic Safety Administration 2009 & 2012, National Association of Insurance Commissioners 2010 & 2011.
}
\references{
http://fivethirtyeight.com/datalab/which-state-has-the-worst-drivers/
}
\examples{
data(bad_drivers_cluster)
StatePlot(2, bad_drivers_cluster) #make a plot of the clusters of the two drivers
}
\keyword{datasets}
