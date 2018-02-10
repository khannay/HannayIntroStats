\name{bgg}
\alias{bgg}
\docType{data}
\title{
Board Game Geek (bgg) Data Set
}
\description{
This data set contains descriptive data and rankings for over 26,000 board games.
}
\usage{data("bgg")}
\format{
  A data frame with 26956 observations on the following 19 variables.
  \describe{
    \item{\code{game.id}}{an integer (discrete) variable giving a unique ID to each board game. This can be used as reference in the orginal data base.}
    \item{\code{details.name}}{a character vector giving the name of the game or expansion}
    \item{\code{game.type}}{a categorical variable giving type of game. It has levels \code{boardgame} \code{boardgameexpansion}}
    \item{\code{details.yearpublished}}{a numeric vector giving the year the game was published starts at 1971-2018 }
    \item{\code{older.2000}}{a categorical variables which gives an answer to the question of whether the game was published before or after the year 2000. It has levels \code{Older than 2000} \code{Younger than 2000}}

    \item{\code{details.minplaytime}}{a numeric vector giving the minimum play time in minutes}
    \item{\code{details.maxplaytime}}{a numeric vector giving the time in minutes for a maximum game}
    \item{\code{details.playingtime}}{a numeric vector the expected play time for the game in minutes}
    \item{\code{game.length}}{a categorical variable giving whether the game takes more or less than an hour in play time. It has levels: \code{Less Than Hour} \code{More than an Hour}}
    \item{\code{details.minplayers}}{a categorical variable giving the min players the game can be played with. It has levels \code{0} \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{10}}
    \item{\code{details.maxplayers}}{A categorical variable giving the max players for the game. It has levels \code{1} \code{10} \code{11+} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} where the 11+ level contains all games with more than 11 possible players}
    \item{\code{game.size}}{a categorical variable telling whether the game requires more than 5 or less than 5 players. It has levels \code{Large Game More than 5} \code{Small Game Less than 5}}
    \item{\code{details.minage}}{a numeric vector giving the minimum suggested age for the game}

    \item{\code{stats.mean}}{a numeric vector giving the average rating for all the people who have rated the game}
    \item{\code{stats.stddev}}{a numeric vector giving the standard deviation of the individual ratings}
    \item{\code{stats.averageweight}}{a numeric vector giving the average complexity weight for the game. Not all users who rated the game also rated the complexity weight. }
    \item{\code{stats.userrated}}{a numeric vector giving the number or users who have rated the game}
    \item{\code{stats.owned}}{a numeric vector giving the number of users who said they own the game. }
    \item{\code{stats.numcomments}}{a numeric vector giving the number of comments posted about the game. }
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Downloaded from \url{https://www.kaggle.com/gabrio/board-games-dataset/data}, although the raw data was scraped from \url{www.boardgamegeek.com}
}
\references{

}
\examples{
data(bgg)
hist(bgg$stats.mean)
}
\keyword{datasets}
