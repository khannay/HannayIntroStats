\name{all_stocks_5yr}
\alias{all_stocks_5yr}
\docType{data}
\title{
S&P Top 500 Stock Price Historical Data
}
\description{
Here I provide a dataset with historical stock prices (last 5 years) for all companies currently found on the S&P 500 index. Each row cooresponds to a day of markety activity.
}
\usage{data("all_stocks_5yr")}
\format{
  A data frame with 619040 observations on the following 7 variables.
  \describe{
    \item{\code{date}}{a Date giving the day the data was recorded for}
    \item{\code{open}}{a numeric vector giving the opening price for the stock}
    \item{\code{high}}{a numeric vector giving the high price of the stock for the day}
    \item{\code{low}}{a numeric vector giving the low value of the stock}
    \item{\code{close}}{a numeric vector giving the closing price of the stock}
    \item{\code{volume}}{a numeric vector giving the number of shares traded}
    \item{\code{Name}}{a factor with the stock ticker name}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
https://www.kaggle.com/camnugent/sandp500
Orginally from The Investor's Exchange api
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
library(dplyr)
data(all_stocks_5yr)
a=filter(all_stocks_5yr, all_stocks_5yr$Name=='AAL') %>% select(date, close)
plot(a$date, a$close)
}
\keyword{datasets}
