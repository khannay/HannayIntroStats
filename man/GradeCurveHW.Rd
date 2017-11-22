\name{GradeCurveHW}
\alias{GradeCurveHW}
\docType{data}
\title{
Learning about Z-scores using Grade Curves
}
\description{
This is a fake data set, giving the exam scores of students. The letter grades are curved using the z scores of the raw grades.  What is wrong with this procedure?
}
\usage{data("GradeCurveHW")}
\format{
  A data frame with 30 observations on the following 3 variables.
  \describe{
    \item{\code{Score}}{a numeric vector giving the raw exam score (out of 100 possible points)}
    \item{\code{z.scores}}{a numeric vetor giving the normalized (z score) for each students grade}
    \item{\code{letter.grades}}{a factor with levels \code{B} \code{C} giving the exam grades based on the z scores. }
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
data(GradeCurveHW)
hist(GradeCurveHW$Score)
table(GradeCurveHW$letter.grades)
}
\keyword{datasets}
