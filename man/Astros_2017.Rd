\name{Astros_2017}
\alias{Astros_2017}
\docType{data}
\title{
The Astros Batting Statistics for 2017 (World Series Year)
}
\description{
This data set has the astros batting statistics for the 2017 regular season. I don't actually know what all the baseball stats mean, but I downloaded this data from basebal-reference.com so more information is available about the meaning of the columns from that website.

}
\usage{data("Astros_2017")}
\format{
  A data frame with 162 observations on the following 33 variables.
  \describe{
    \item{\code{Rk}}{a numeric vector}
    \item{\code{Gtm}}{a numeric vector}
    \item{\code{Date}}{a character vector}
    \item{\code{Opp}}{a factor with levels \code{ARI} \code{ATL} \code{BAL} \code{BOS} \code{CHW} \code{CLE} \code{DET} \code{KCR} \code{LAA} \code{MIA} \code{MIN} \code{NYM} \code{NYY} \code{OAK} \code{PHI} \code{SEA} \code{TBR} \code{TEX} \code{TOR} \code{WSN}}
    \item{\code{Rslt}}{a character vector}
    \item{\code{PA}}{a numeric vector}
    \item{\code{AB}}{a numeric vector}
    \item{\code{R}}{a numeric vector}
    \item{\code{H}}{a numeric vector}
    \item{\samp{2B}}{a numeric vector}
    \item{\samp{3B}}{a numeric vector}
    \item{\code{HR}}{a numeric vector}
    \item{\code{RBI}}{a numeric vector}
    \item{\code{BB}}{a numeric vector}
    \item{\code{IBB}}{a numeric vector}
    \item{\code{SO}}{a numeric vector}
    \item{\code{HBP}}{a numeric vector}
    \item{\code{SH}}{a numeric vector}
    \item{\code{SF}}{a numeric vector}
    \item{\code{ROE}}{a numeric vector}
    \item{\code{GDP}}{a numeric vector}
    \item{\code{SB}}{a numeric vector}
    \item{\code{CS}}{a numeric vector}
    \item{\code{BA}}{a numeric vector}
    \item{\code{OBP}}{a numeric vector}
    \item{\code{SLG}}{a numeric vector}
    \item{\code{OPS}}{a numeric vector}
    \item{\code{LOB}}{a numeric vector}
    \item{\samp{#}}{a numeric vector}
    \item{\code{Thr}}{a factor with levels \code{L} \code{R}}
    \item{\samp{Opp. Starter (GmeSc)}}{a character vector}
    \item{\code{Game.Result}}{a factor with levels \code{L} \code{W}}
    \item{\code{Game.Location}}{a character vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
https://www.baseball-reference.com/teams/tgl.cgi?team=HOU&t=b&year=2017
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Astros_2017)
}
\keyword{datasets}
