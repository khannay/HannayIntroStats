\name{Gambling_HW_Normality}
\alias{Gambling_HW_Normality}
\docType{data}
\title{
Accessing the Normality of Gambling Winnings
}
\description{
This is a fake data set giving gambling winnings for two players (Kevin and Mark). You can assess the normality of the winnings of both players and each player individually. This can also be used as an example of statistical hypothesis testing.
}
\usage{data("Gambling_HW_Normality")}
\format{
  A data frame with 100 observations on the following 2 variables.
  \describe{
    \item{\code{Player}}{a factor with levels \code{Kevin} \code{Mark}}
    \item{\code{Winnings}}{a numeric vector giving the winnings in dollars for that round of bets}
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
data(Gambling_HW_Normality)
boxplot(Gambling_HW_Normality$Winnings~Gambling_HW_Normality$Player) #make a boxplot of each players winnings individually
}
\keyword{datasets}
