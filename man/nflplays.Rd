\name{nflplays}
\alias{nflplays}
\docType{data}
\title{
NFL Plays from 2009-2017
}
\description{
This data set contains all run/pass plays from NFL seasons 2009-2017. Each row represents one play and the columns give information about when the play occured and the results of the play.
}
\usage{data("nflplays")}
\format{
  A data frame with 275791 observations on the following 43 variables.
  \describe{
    \item{\code{Date}}{the date of the game}
    \item{\code{GameID}}{a numeric value which is unique for each game in the data set}
    \item{\code{Drive}}{this numbers the times each team had the ball. It will increase by one each time the ball changes hands in the game}
    \item{\code{qtr}}{the game quarter the play took place in (5 means overtime)}
    \item{\code{down}}{the down of the play 1-4. }
    \item{\code{time}}{this gives the time on the game clock for that quarter (15 minutes) when the play started}
    \item{\code{PlayTimeDiff}}{a numeric vector}
    \item{\code{yrdline100}}{a numeric vector giving the yard line the play started from}
    \item{\code{ydstogo}}{a numeric vector giving the yards to go for a first down}
    \item{\code{ydsnet}}{a numeric vector this gives the total number of yards accumulated on the drive}
    \item{\code{GoalToGo}}{a numeric vector this tells whether the team has no first downs left before a touchdown}
    \item{\code{posteam}}{a factor giving the team with the ball with levels \code{ARI} \code{ATL} \code{BAL} \code{BUF} \code{CAR} \code{CHI} \code{CIN} \code{CLE} \code{DAL} \code{DEN} \code{DET} \code{GB} \code{HOU} \code{IND} \code{JAC} \code{JAX} \code{KC} \code{LA} \code{LAC} \code{MIA} \code{MIN} \code{NE} \code{NO} \code{NYG} \code{NYJ} \code{OAK} \code{PHI} \code{PIT} \code{SD} \code{SEA} \code{SF} \code{STL} \code{TB} \code{TEN} \code{WAS}}
    \item{\code{DefensiveTeam}}{a factor giving the team playing defense with levels \code{ARI} \code{ATL} \code{BAL} \code{BUF} \code{CAR} \code{CHI} \code{CIN} \code{CLE} \code{DAL} \code{DEN} \code{DET} \code{GB} \code{HOU} \code{IND} \code{JAC} \code{JAX} \code{KC} \code{LA} \code{LAC} \code{MIA} \code{MIN} \code{NE} \code{NO} \code{NYG} \code{NYJ} \code{OAK} \code{PHI} \code{PIT} \code{SD} \code{SEA} \code{SF} \code{STL} \code{TB} \code{TEN} \code{WAS}}
    \item{\code{Yards.Gained}}{a numeric vector giving the net yards for the play}
    \item{\code{sp}}{a numeric vector tells if the play was a scoring play 0=No, 1=Yes}
    \item{\code{Touchdown}}{a numeric vector tells if the play was a touchdown 0=No, 1=Yes}
    \item{\code{Safety}}{a numeric vector tells if the play was a safety (two points for the defense) 0=No, 1=Yes}
    \item{\code{PlayType}}{Tells if the play was a Run or a Pass a factor with levels \code{Pass} \code{Run}}
    \item{\code{Passer}}{a factor giving the name of the player throwing the Pass, generally a Quarterback}
    \item{\code{PassOutcome}}{a factor giving the result of the pass play \code{Complete} \code{Incomplete Pass}}
    \item{\code{PassLength}}{a factor with levels \code{20} \code{Deep} \code{Short} tells the distance of the pass as a categorical variable}
    \item{\code{AirYards}}{a numeric vector giving the number of yards the pass traveled in the air}
    \item{\code{YardsAfterCatch}}{a numeric vector giving the yards gained after the player caught the ball}
    \item{\code{QBHit}}{a factor with levels \code{0} \code{1} telling whether the QB was hit during the play}
    \item{\code{PassLocation}}{a factor with levels \code{left} \code{middle} \code{right} giving the direction of the pass}
    \item{\code{InterceptionThrown}}{a factor with levels \code{0} \code{1} 0=No interception, 1=interception}
    \item{\code{Interceptor}}{a character vector gives the name of the player who intercepeted the ball. }
    \item{\code{Rusher}}{a factor giving the name of the rusher (running play)}
    \item{\code{RunLocation}}{a factor with levels \code{left} \code{middle} \code{right} gives the direcion of the run}
    \item{\code{RunGap}}{a factor with levels \code{end} \code{guard} \code{tackle} gives the football version of the direction}
    \item{\code{Receiver}}{a factor giving the name of the reciever on a pass play}
    \item{\code{Reception}}{a factor with levels \code{0} \code{1} tells if the player caught the ball}
    \item{\code{Fumble}}{a numeric vector giving whether the play had a fumble 0=No, 1=Yes}
    \item{\code{RecFumbTeam}}{a character vector which team recovered the fumble}
    \item{\code{Sack}}{a numeric vector telling if the QB was sacked 0=No, 1=Yes}
    \item{\code{PosTeamScore}}{a numeric vector with the game score of the team with the ball}
    \item{\code{DefTeamScore}}{a numeric vector with the game score of the team on defense}
    \item{\code{ScoreDiff}}{a numeric vector giving the difference in the game score}
    \item{\code{AbsScoreDiff}}{a numeric vector giving the absolute difference in the game score}
    \item{\code{HomeTeam}}{a factor giving the home team with levels \code{ARI} \code{ATL} \code{BAL} \code{BUF} \code{CAR} \code{CHI} \code{CIN} \code{CLE} \code{DAL} \code{DEN} \code{DET} \code{GB} \code{HOU} \code{IND} \code{JAC} \code{JAX} \code{KC} \code{LA} \code{LAC} \code{MIA} \code{MIN} \code{NE} \code{NO} \code{NYG} \code{NYJ} \code{OAK} \code{PHI} \code{PIT} \code{SD} \code{SEA} \code{SF} \code{STL} \code{TB} \code{TEN} \code{WAS}}
    \item{\code{AwayTeam}}{a factor giving the away team with levels \code{ARI} \code{ATL} \code{BAL} \code{BUF} \code{CAR} \code{CHI} \code{CIN} \code{CLE} \code{DAL} \code{DEN} \code{DET} \code{GB} \code{HOU} \code{IND} \code{JAC} \code{JAX} \code{KC} \code{LA} \code{LAC} \code{MIA} \code{MIN} \code{NE} \code{NO} \code{NYG} \code{NYJ} \code{OAK} \code{PHI} \code{PIT} \code{SD} \code{SEA} \code{SF} \code{STL} \code{TB} \code{TEN} \code{WAS}}
    \item{\code{Season}}{a factor giving the season of the play with levels \code{2009} \code{2010} \code{2011} \code{2012} \code{2013} \code{2014} \code{2015} \code{2016} \code{2017}}
    \item{\code{play.description}}{a character vector giving the play in sentence form}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
https://github.com/ryurko/nflscrapR-data
Scraped from nfl.com
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(nflplays)

}
\keyword{datasets}
