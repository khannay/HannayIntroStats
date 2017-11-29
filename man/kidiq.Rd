\name{kidiq}
\alias{kidiq}
\docType{data}
\title{
IQ Score Prediction
}
\description{
This data set can be used in a regression analysis to try and predict the IQ scores of children given characteristics of their mothers.
}
\usage{data("kidiq")}
\format{
  A data frame with 434 observations on the following 5 variables.
  \describe{
    \item{\code{kid_score}}{a numeric vector giving the childs IQ score}
    \item{\code{mom_hs}}{a numeric value either 1 or 0 giving whether the mom finished high school}
    \item{\code{mom_iq}}{a numeric value giving the moms IQ score}
    \item{\code{mom_work}}{a numeric code giving the type of work done by the mother either 1,2,3 or 4}
    \item{\code{mom_age}}{a numeric vale giving the age of the mother when the child was born}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
National Longitudinal Survey of Youth
}
\references{
Taken from Gelman and Hill's book on regression analysis
}
\examples{
data(kidiq)
}
\keyword{datasets}
