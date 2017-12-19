\name{wine_tasting}
\alias{wine_tasting}
\docType{data}
\title{
Wine Tasting Data Set
}
\description{
The data was scraped from WineEnthusiast during the week of June 15th, 2017. It contains information about the wine as well as the rating and price of a bottle of the wine.

This data set has had all missing values removed.
}
\usage{data("wine_tasting")}
\format{
  A data frame with 96420 observations on the following 8 variables.
  \describe{
    \item{\code{country}}{a factor giving the country of origin for the wine }
    \item{\code{description}}{a character vector giving the wine review/description for that bottle of wine. }
    \item{\code{price}}{a numeric vector giving the cost of the wine in dollars. }
    \item{\code{province}}{a factor giving the province or state that the wine is from. }
    \item{\code{taster_name}}{a factor giving the tasters name}
    \item{\code{winery}}{a factor giving the particular winery the wine came from}
    \item{\code{Rating}}{a numeric vector giving the points WineEnthusiast rated the wine on a scale of 1-100 (though they say they only post reviews for wines that score >=80) }
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Wine Enthusiast
\url{https://www.kaggle.com/zynicide/wine-reviews}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(wine_tasting)
w2=dropLowFactors(wine_tasting, factor.column = 1, threshold = 1000) #drop wines from countries with less than 1000 wines in data
boxplot(w2$price~w2$country)
}
\seealso{
\code{dropLowFactors()}
}
\keyword{datasets}
