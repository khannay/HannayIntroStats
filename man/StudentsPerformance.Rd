\name{StudentsPerformance}
\alias{StudentsPerformance}
\docType{data}
\title{
Students Performance in Exams
}
\description{
his data set consists of the marks secured by the students in various subjects.
}
\usage{data("StudentsPerformance")}
\format{
  A data frame with 1000 observations on the following 8 variables.
  \describe{
    \item{\code{gender}}{a factor with levels \code{female} \code{male}}
    \item{\code{lunch}}{a factor with levels \code{free/reduced} \code{standard}}
    \item{\code{race}}{a factor with levels \code{group A} \code{group B} \code{group C} \code{group D} \code{group E}}
    \item{\code{Parents.Education}}{a factor with levels \code{associate's degree} \code{bachelor's degree} \code{high school} \code{master's degree} \code{some college} \code{some high school}}
    \item{\code{Test.Prep.Course}}{a factor with levels \code{completed} \code{none}}
    \item{\code{Math.Score}}{a numeric vector}
    \item{\code{Reading.Score}}{a numeric vector}
    \item{\code{Writing.Score}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
http://roycekimmons.com/tools/generated_data/exams
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(StudentsPerformance)
plot(StudentsPerformance$Math.Score, StudentsPerformance$Reading.Score)
}
\keyword{datasets}
