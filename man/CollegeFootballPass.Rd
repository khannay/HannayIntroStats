\name{CollegeFootballPass}
\alias{CollegeFootballPass}
\docType{data}
\title{
Data for Pass Plays in College Football from 2005-2013
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("CollegeFootballPass")}
\format{
  A data frame with 455082 observations on the following 15 variables.
  \describe{
    \item{\code{Game_ID}}{a character vector}
    \item{\code{Play_Number}}{a numeric vector}
    \item{\code{Passer_Code}}{a numeric vector}
    \item{\code{Rec_Code}}{a numeric vector}
    \item{\code{Attempt}}{a numeric vector}
    \item{\code{Completion}}{a numeric vector}
    \item{\code{Yards}}{a numeric vector}
    \item{\code{Touchdown}}{a numeric vector}
    \item{\code{Interception}}{a numeric vector}
    \item{\code{First.Down}}{a numeric vector}
    \item{\code{Dropped}}{a numeric vector}
    \item{\code{Game.Date}}{a character vector}
    \item{\code{Offense_team}}{a character vector}
    \item{\code{Defense_team}}{a character vector}
    \item{\code{Home_Away}}{a character vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
https://www.kaggle.com/mhixon/college-football-statistics
}
\references{
Thanks to cbfstats.com and J. Albert Bowden II for the dataset.
}
\examples{
data(CollegeFootballPass)
## maybe str(CollegeFootballPass) ; plot(CollegeFootballPass) ...
}
\keyword{datasets}
