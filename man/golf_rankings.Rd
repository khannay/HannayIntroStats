\name{golf_rankings}
\alias{golf_rankings}
\docType{data}
\title{
Golf Rankings Historical Data
}
\description{
Rankings are a constant phenomenon in society, with a persistent interest in the stratification of items in a set across various disciplines. In sports, rankings are a direct representation of the performance of a team or player over a certain period. Given the straightforward nature of rankings in sports (points based system) there is the opportunity to statistically explore rankings of sports disciplines.
}
\usage{data("golf_rankings")}
\format{
  A data frame with 758000 observations on the following 4 variables.
  \describe{
    \item{\code{date}}{a Date}
    \item{\code{rank}}{a numeric vector}
    \item{\code{name}}{a factor with many levels giving the players name}
    \item{\code{avg_points}}{a numeric vector}
  }
}
\details{
The dataset comprises weekly rankings data of the Top 1000 golf players between Sep 2000 and June April 2015.
}
\source{
https://www.kaggle.com/odartey/top-1000-golf-players-historical
}
\references{
Data was sourced from the Official Golf World Rankings (OGWR) website: ogwr.com
}
\examples{
data(golf_rankings)
a=subset(ogwr_historical, ogwr_historical$name=="TigerWoods")
plot(a$date, a$rank)
}
\keyword{datasets}
