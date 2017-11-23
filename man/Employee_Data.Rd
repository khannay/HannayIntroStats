\name{Employee_Data}
\alias{Employee_Data}
\docType{data}
\title{
Employee Performance Score Data
}
\description{
Fake data set giving the performance scores of employees at a company.
}
\usage{data("Employee_Data")}
\format{
  A data frame with 100 observations on the following 2 variables.
  \describe{
    \item{\code{Employee.ID}}{numeric vector giving the id, an integer between 1 to 100 or the employee.}
    \item{\code{Performance.Score}}{The employees performance score on a range from 0 to 100}
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
data(Employee_Data)
boxplot(Employee_Data$Performance.Score)
hist(Employee_Data$Performance.Score)
}
\keyword{datasets}
