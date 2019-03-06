\name{videoGameSales}
\alias{videoGameSales}
\docType{data}
\title{
Video Game Sales Data through Jan 2017
}
\description{
This data set contains a list of video games with sales greater than 100,000 copies along with critic and user ratings. Scraped from VGChartz and MetaCritic.
}
\usage{data("videoGameSales")}
\format{
  A data frame with 17416 observations on the following 15 variables.
  \describe{
    \item{\code{Name}}{a character vector}
    \item{\code{Platform}}{a factor with many levels giving the gaming system}
    \item{\code{Year_of_Release}}{a factor with years from 1976-2017}
    \item{\code{Genre}}{a factor with levels \code{Action} \code{Adventure} \code{Fighting} \code{Misc} \code{Platform} \code{Puzzle} \code{Racing} \code{Role-Playing} \code{Shooter} \code{Simulation} \code{Sports} \code{Strategy}}
    \item{\code{Publisher}}{a factor with many levels showing the creating company}
    \item{\code{NA_Sales}}{a numeric vector}
    \item{\code{EU_Sales}}{a numeric vector}
    \item{\code{JP_Sales}}{a numeric vector}
    \item{\code{Other_Sales}}{a numeric vector}
    \item{\code{Global_Sales}}{a numeric vector}
    \item{\code{Critic_Score}}{a numeric vector}
    \item{\code{Critic_Count}}{a numeric vector}
    \item{\code{User_Score}}{a numeric vector}
    \item{\code{User_Count}}{a numeric vector}
    \item{\code{Rating}}{a character vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
https://www.kaggle.com/kendallgillies/video-game-sales-and-ratings#Video_Game_Sales_as_of_Jan_2017.csv
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(videoGameSales)
## maybe str(videoGameSales) ; plot(videoGameSales) ...
}
\keyword{datasets}
