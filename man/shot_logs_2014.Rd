\name{shot_logs_2014}
\alias{shot_logs_2014}
\docType{data}
\title{
NBA Shot Selection for 2014-2015 Season
}
\description{
Data on shots taken during the 2014-2015 season, who took the shot, where on the floor was the shot taken from, who was the nearest defender, how far away was the nearest defender, time on the shot clock, and much more. The column titles are generally self-explanatory.

Note this is most the data for that NBA season. It looks like it has all games until about 3/4/2015. Covering 896 games.

Useful for evaluating who the best shooter is, who the best defender is, the hot-hand hypothesis, etc.

Scraped from NBA's REST API.
}
\usage{data("shot_logs_2014")}
\format{
  A data frame with 122502 observations on the following 21 variables.
  \describe{
    \item{\code{GAME_ID}}{a numeric vector, unique ID for each game}
    \item{\code{MATCHUP}}{a factor with levels giving the date and teams in a matchup}
    \item{\code{LOCATION}}{a factor with levels \code{A} \code{H} giving whether the shot was taken by the home team or away team}
    \item{\code{W}}{factor giving whether the team shooting won or lost the game \code{L} \code{W}}
    \item{\code{FINAL_MARGIN}}{Final Margin of the Game, numeric value}
    \item{\code{SHOT_NUMBER}}{a numeric vector, order of the shots in the game}
    \item{\code{PERIOD}}{Gives the period the shot was taken in 1,2,3,4, Overtime}
    \item{\code{GAME_CLOCK}}{Time on the game clock when the shot was taken}
    \item{\code{SHOT_CLOCK}}{Time remaining on the shot clock when the shot was taken}
    \item{\code{DRIBBLES}}{Number of dribbles before shooting}
    \item{\code{TOUCH_TIME}}{seconds the player had the ball before shooting}
    \item{\code{SHOT_DIST}}{Distance the shot was taken from in feet}
    \item{\code{PTS_TYPE}}{two or three point shot}
    \item{\code{SHOT_RESULT}}{a factor with levels \code{made} \code{missed} giving the results of the shot}
    \item{\code{CLOSEST_DEFENDER}}{a factor with many levels giving the name of player on the other team closest to the player taking the shot}
    \item{\code{CLOSEST_DEFENDER_PLAYER_ID}}{a numeric vector}
    \item{\code{CLOSE_DEF_DIST}}{a numeric vector giving the distance the defender is from the player}
    \item{\code{FGM}}{1 or 0 depending if the shot went in}
    \item{\code{PTS}}{Points from the shot (redundant)}
    \item{\code{player_name}}{a character vector gives the shooting players name}
    \item{\code{player_id}}{a numeric vector of each players unique identifier}
    \item{\code{Home.Team}}{a categorical variable giving the home team for that game}
    \item{\code{Away.Team}}{a categorical variable giving the away team for that game}
    \item{\code{Team.Shooting}}{a categorical variable giving the team which took the shot.}
    \item{\code{Date.Of.Game}}{a date giving the date the game was player on}
    \item{\code{Team.Defending}}{a categorical variable giving the team playing defense for that shot}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(shot_logs_2014)
}
\keyword{datasets}
