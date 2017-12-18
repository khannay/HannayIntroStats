\name{NBA_Salaries_2017}
\alias{NBA_Salaries_2017}
\docType{data}
\title{
NBA Salaries 2017-2018
}
\description{
This data set gives the salaries of all NBA players during the 2017-2018 season.
}
\usage{data("NBA_Salaries_2017")}
\format{
  A data frame with 512 observations on the following 10 variables.
  \describe{
    \item{\code{Player}}{a character vector giving the players names}
    \item{\code{Tm}}{The players team, stored as a factor}
    \item{\code{X2017.18}}{a numeric vector giving the salary 2017-2018}
    \item{\code{X2018.19}}{a numeric vector giving the salary 2018-2019}
    \item{\code{X2019.20}}{a numeric vector giving the salary 2019-2020}
    \item{\code{X2020.21}}{a numeric vector giving the salary 2020-2021}
    \item{\code{X2021.22}}{a numeric vector giving the salary 2021-2022}
    \item{\code{X2022.23}}{a numeric vector giving the salary 2022-2023}
    \item{\code{Signed.Using}}{ a factor giving the method used to sign the player}
    \item{\code{Guaranteed}}{a numeric vector giving the total amount of salary guaranteed}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
\link{https://www.basketball-reference.com/contracts/players.html}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(NBA_Salaries_2017)
}
\keyword{datasets}
