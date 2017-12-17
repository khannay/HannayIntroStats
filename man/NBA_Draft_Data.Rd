\name{NBA_Draft_Data}
\alias{NBA_Draft_Data}
\docType{data}
\title{
NBA Draft Data and Career Stats
}
\description{
This data set has all first round NBA draft picks from 1990-2017 with their career stats through 2018.
}
\usage{data("NBA_Draft_Data")}
\format{
  A data frame with 737 observations on the following 23 variables.
  \describe{
    \item{\code{Tm}}{factor giving the team who drafted the player}
    \item{\code{Player}}{a character vector giving the players name}
    \item{\code{Age}}{a numeric vector giving the age of the player when drafted.}
    \item{\code{Pos}}{a factor with levels \code{C} \code{C-F} \code{F} \code{F-C} \code{F-G} \code{G} \code{G-F} giving the position played by the player. }
    \item{\code{Born}}{a factor giving the country the player was born. }
    \item{\code{College}}{ a facto giving the college of the player (if they went to college)}
    \item{\code{From}}{Start year of career}
    \item{\code{To}}{End year of career}
    \item{\code{G}}{a numeric vector giving the number of games played in their career}
    \item{\code{MP}}{a numeric vector giving the average minutes played}
    \item{\code{PTS}}{a numeric vector giving career PPG average}
    \item{\code{TRB}}{a numeric vector giving the career total rebound average}
    \item{\code{AST}}{a numeric vector giving the career assist average}
    \item{\code{STL}}{a numeric vector giving the career steals average}
    \item{\code{BLK}}{a numeric vector giving the career blocks average}
    \item{\samp{FG\%}}{a numeric vector giving the FG percentage}
    \item{\samp{2P\%}}{a numeric vector giving the FG percentage on two point shots.}
    \item{\samp{3P\%}}{a numeric vector giving the FG percentage on three point shots.}
    \item{\samp{FT\%}}{a numeric vector givng the career FT percentage.}
    \item{\code{WS}}{a numeric vector giving the career win shares}
    \item{\samp{WS/48}}{a numeric vector giving the career win shares per 48 minutes}
    \item{\code{Draft.Year}}{a numeric vector year the player was drafted}
    \item{\code{Pick.Number}}{a numeric vector giving the pick number}
  }
}
\details{
NA
}
\source{
\link{https://www.basketball-reference.com/draft/}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(NBA_Draft_Data)
}
\keyword{datasets}
