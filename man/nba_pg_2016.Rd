\name{nba_pg_2016}
\alias{nba_pg_2016}
\docType{data}
\title{
NBA Per Game Statistics for 2016-2017 (Top 100 Players in PPG)
}
\description{
This data set gives the per game statistics of the top 100 NBA players during the 2016-2017 season. The row names of the data frame give the players names, all other entries are numeric. This is mostly intended as a example for clustering analysis.
}
\usage{data("nba_pg_2016")}
\format{
  A data frame with 100 observations on the following 22 variables.
  \describe{
    \item{\code{FG}}{a numeric vector, average number of field goals made}
    \item{\code{FGA}}{a numeric vector, average number of field goal attempts per game}
    \item{\code{FG.}}{a numeric vector, field goal percentage}
    \item{\code{X3P}}{a numeric vector, average three point makes per game}
    \item{\code{X3PA}}{a numeric vector, average three point attempts per game}
    \item{\code{X3P.}}{a numeric vector, average three point percentage per game}
    \item{\code{X2P}}{a numeric vector, average 2 point makes per game}
    \item{\code{X2PA}}{a numeric vector, average 2 point attempts per game}
    \item{\code{X2P.}}{a numeric vector, average 2 point percentage}
    \item{\code{eFG.}}{a numeric vector}
    \item{\code{FT}}{a numeric vector, average free throws made per game}
    \item{\code{FTA}}{a numeric vector, average free throw attempts per game}
    \item{\code{FT.}}{a numeric vector, free thow shooting percentage}
    \item{\code{ORB}}{a numeric vector, offensive rebounds per game}
    \item{\code{DRB}}{a numeric vector, defensive rebounds per game}
    \item{\code{TRB}}{a numeric vector, total rebounds per game}
    \item{\code{AST}}{a numeric vector, average assists per game}
    \item{\code{STL}}{a numeric vector, average steals per game}
    \item{\code{BLK}}{a numeric vector, average blocks per game}
    \item{\code{TOV}}{a numeric vector, average turnovers per game}
    \item{\code{PF}}{a numeric vector, average player fouls per game}
    \item{\code{PPG}}{a numeric vector, average points per game}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
Downloaded and formated from Basketball Reference dot com.
}
\examples{
data(nba_pg_2016)
kmeans(nba_pg_2016, centers=2, nstarts=25)
}
\keyword{datasets}
