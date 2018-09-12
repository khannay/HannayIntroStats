\name{baseball_hometeam_stats_2016}
\alias{baseball_hometeam_stats_2016}
\docType{data}
\title{
Baseball Game Data
}
\description{
This data set has information on the every game during the 2016 MLB season, including the playoffs.
}
\usage{data("baseball_hometeam_stats_2016")}
\format{
  A data frame with 2463 observations on the following 24 variables.
  \describe{
    \item{\code{attendance}}{a numeric vector giving the number of fans who attended the game}
    \item{\code{away_team}}{a factor giving the away team name}
    \item{\code{away_team_errors}}{a numeric vector giving the number of errors commited during the game}
    \item{\code{away_team_hits}}{a numeric vector giving the number of hits for the away team}
    \item{\code{away_team_runs}}{a numeric vector giving the away team runs scored}
    \item{\code{date}}{a Date giving the date of the game}
    \item{\code{field_type}}{a factor with levels \code{on grass} \code{on turf}}
    \item{\code{game_type}}{a factor with levels \code{Day Game} \code{Night Game}}
    \item{\code{home_team}}{a factor giving the home team name with levels \code{Arizona Diamondbacks} \code{Atlanta Braves} \code{Baltimore Orioles} \code{Boston Red Sox} \code{Chicago Cubs} \code{Chicago White Sox} \code{Cincinnati Reds} \code{Cleveland Indians} \code{Colorado Rockies} \code{Detroit Tigers} \code{Houston Astros} \code{Kansas City Royals} \code{Los Angeles Angels of Anaheim} \code{Los Angeles Dodgers} \code{Miami Marlins} \code{Milwaukee Brewers} \code{Minnesota Twins} \code{New York Mets} \code{New York Yankees} \code{Oakland Athletics} \code{Philadelphia Phillies} \code{Pittsburgh Pirates} \code{San Diego Padres} \code{San Francisco Giants} \code{Seattle Mariners} \code{St. Louis Cardinals} \code{Tampa Bay Rays} \code{Texas Rangers} \code{Toronto Blue Jays} \code{Washington Nationals}}
    \item{\code{home_team_errors}}{a numeric vector}
    \item{\code{home_team_hits}}{a numeric vector}
    \item{\code{home_team_runs}}{a numeric vector}
    \item{\code{start_time}}{a character vector}
    \item{\code{venue}}{a character vector}
    \item{\code{day_of_week}}{a factor with levels \code{Friday} \code{Monday} \code{Saturday} \code{Sunday} \code{Thursday} \code{Tuesday} \code{Wednesday}}
    \item{\code{temperature}}{a numeric vector}
    \item{\code{wind_speed}}{a numeric vector}
    \item{\code{wind_direction}}{a factor with levels \code{from Left to Right} \code{from Right to Left} \code{in from Centerfield} \code{in from Leftfield} \code{in from Rightfield} \code{in unknown direction} \code{out to Centerfield} \code{out to Leftfield} \code{out to Rightfield}}
    \item{\code{sky}}{a factor with levels \code{Cloudy} \code{Drizzle} \code{In Dome} \code{Night} \code{Overcast} \code{Rain} \code{Sunny} \code{Unknown}}
    \item{\code{total_runs}}{a numeric vector}
    \item{\code{game_hours_dec}}{a numeric vector}
    \item{\code{season}}{a factor with levels \code{post season} \code{regular season}}
    \item{\code{home_team_win}}{a numeric vector, 1=win, 0=loss}
    \item{\code{home_team_outcome}}{a factor (Win and Loss)}
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
data(baseball_hometeam_stats_2016)
## maybe str(baseball_hometeam_stats_2016) ; plot(baseball_hometeam_stats_2016) ...
}
\keyword{datasets}
