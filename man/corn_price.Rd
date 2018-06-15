\name{corn_price}
\alias{corn_price}
\docType{data}
\title{
Weekly Corn Prices
}
\description{
The close price of corn over a period from 2013-2017.
}
\usage{data("corn_price")}
\format{
  A data frame with 248 observations on the following 2 variables.
  \describe{
    \item{\code{date}}{a date giving the weekend of that week}
    \item{\code{price}}{a numeric vector giving the closing price for corn that week}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Quantopian corn futures price.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(corn_price)
scatter(corn_price$date, corn_price$price)
}
\keyword{datasets}
