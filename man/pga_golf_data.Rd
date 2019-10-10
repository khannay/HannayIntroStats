\name{pga_golf_data}
\alias{pga_golf_data}
\docType{data}
\title{
PGA Golf Player Data
}
\description{

}This file contains data about PGA Tour players from 2010 to 2018
\usage{data("pga_golf_data")}
\format{
  A data frame with 2312 observations on the following 18 variables.
  \describe{
    \item{\code{Player_Name}}{a character vector}
    \item{\code{Rounds}}{a numeric vector}
    \item{\code{Fairway_Percentage}}{a numeric vector}
    \item{\code{Year}}{a numeric vector}
    \item{\code{Avg_Distance}}{a numeric vector}
    \item{\code{gir}}{a numeric vector}
    \item{\code{Average_Putts}}{a numeric vector}
    \item{\code{Average_Scrambling}}{a numeric vector}
    \item{\code{Average_Score}}{a numeric vector}
    \item{\code{Points}}{a numeric vector}
    \item{\code{Wins}}{a numeric vector}
    \item{\code{Top_10}}{a numeric vector}
    \item{\code{Average_SG_Putts}}{a numeric vector}
    \item{\code{Average_SG_Total}}{a numeric vector}
    \item{\samp{SG:OTT}}{a numeric vector}
    \item{\samp{SG:APR}}{a numeric vector}
    \item{\samp{SG:ARG}}{a numeric vector}
    \item{\code{Money}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
https://www.kaggle.com/jmpark746/pga-tour-data-2010-2018
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(pga_golf_data)
## maybe str(pga_golf_data) ; plot(pga_golf_data) ...
}
\keyword{datasets}
