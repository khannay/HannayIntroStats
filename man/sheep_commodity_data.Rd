\name{sheep_commodity_data}
\alias{sheep_commodity_data}
\docType{data}
\title{
Sheep Commodity Data
}
\description{
This data set has sheep import and export data for the last 30 years (with many missing years).
}
\usage{data("sheep_commodity_data")}
\format{
  A data frame with 2369 observations on the following 6 variables.
  \describe{
    \item{\code{country_or_area}}{a character vector giving the country name}
    \item{\code{year}}{a numeric vector giving the year}
    \item{\code{flow}}{a factor giving the direction of the flow: Import, Export, Re-Import, Re-Export}
    \item{\code{trade_usd}}{a numeric vector giving the value of the flow in US dolalrs}
    \item{\code{weight_kg}}{a numeric vector giving the total weight of shipped goods}
    \item{\code{quantity}}{a numeric vector giving the number of live sheep moved}
  }
}
\details{


sheep_commodity_data=dplyr::filter(commodity_trade_statistics_data, grepl('Sheep', commodity), category=='01_live_animals', quantity_name=='Number of items')

}
\source{
This dataset was kindly published by the United Nations Statistics Division on the UNData site. I used the processed form of this data from
https://www.kaggle.com/unitednations/global-commodity-trade-statistics
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(sheep_commodity_data)
}
\keyword{datasets}
