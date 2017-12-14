\name{HR_Employee_Attrition}
\alias{HR_Employee_Attrition}
\docType{data}
\title{
HR Employee Attrition Data Set
}
\description{
Uncover the factors that lead to employee attrition and explore important questions such as ‘show me a breakdown of distance from home by job role and attrition’ or ‘compare average monthly income by education and attrition’. This is a fictional data set created by IBM data scientists. Most of the columns should be self explanatory. This data set should be good practice for business majors who may be tasked with finding trends in a data set like this one.
}
\usage{data("HR_Employee_Attrition")}
\format{
  A data frame with 1470 observations on the following 35 variables.
  \describe{
    \item{\code{Age}}{a numeric vector}
    \item{\code{Attrition}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{BusinessTravel}}{a factor with levels \code{Non-Travel} \code{Travel_Frequently} \code{Travel_Rarely}}
    \item{\code{DailyRate}}{a numeric vector}
    \item{\code{Department}}{a factor with levels \code{Human Resources} \code{Research & Development} \code{Sales}}
    \item{\code{DistanceFromHome}}{a numeric vector}
    \item{\code{Education}}{a numeric vector}
    \item{\code{EducationField}}{a factor with levels \code{Human Resources} \code{Life Sciences} \code{Marketing} \code{Medical} \code{Other} \code{Technical Degree}}
    \item{\code{EmployeeCount}}{a numeric vector}
    \item{\code{EmployeeNumber}}{a numeric vector}
    \item{\code{EnvironmentSatisfaction}}{a numeric vector}
    \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{HourlyRate}}{a numeric vector}
    \item{\code{JobInvolvement}}{a numeric vector}
    \item{\code{JobLevel}}{a numeric vector}
    \item{\code{JobRole}}{a factor with levels \code{Healthcare Representative} \code{Human Resources} \code{Laboratory Technician} \code{Manager} \code{Manufacturing Director} \code{Research Director} \code{Research Scientist} \code{Sales Executive} \code{Sales Representative}}
    \item{\code{JobSatisfaction}}{a numeric vector}
    \item{\code{MaritalStatus}}{a factor with levels \code{Divorced} \code{Married} \code{Single}}
    \item{\code{MonthlyIncome}}{a numeric vector}
    \item{\code{MonthlyRate}}{a numeric vector}
    \item{\code{NumCompaniesWorked}}{a numeric vector}
    \item{\code{Over18}}{a factor with levels \code{Y}}
    \item{\code{OverTime}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{PercentSalaryHike}}{a numeric vector}
    \item{\code{PerformanceRating}}{a numeric vector}
    \item{\code{RelationshipSatisfaction}}{a numeric vector}
    \item{\code{StandardHours}}{a numeric vector}
    \item{\code{StockOptionLevel}}{a numeric vector}
    \item{\code{TotalWorkingYears}}{a numeric vector}
    \item{\code{TrainingTimesLastYear}}{a numeric vector}
    \item{\code{WorkLifeBalance}}{a numeric vector}
    \item{\code{YearsAtCompany}}{a numeric vector}
    \item{\code{YearsInCurrentRole}}{a numeric vector}
    \item{\code{YearsSinceLastPromotion}}{a numeric vector}
    \item{\code{YearsWithCurrManager}}{a numeric vector}
  }
}
\details{
Education 1 'Below College' 2 'College' 3 'Bachelor' 4 'Master' 5 'Doctor'

EnvironmentSatisfaction 1 'Low' 2 'Medium' 3 'High' 4 'Very High'

JobInvolvement
1 'Low' 2 'Medium' 3 'High' 4 'Very High'

JobSatisfaction 1 'Low' 2 'Medium' 3 'High' 4 'Very High'

PerformanceRating
1 'Low' 2 'Good' 3 'Excellent' 4 'Outstanding'

RelationshipSatisfaction
1 'Low' 2 'Medium' 3 'High' 4 'Very High'

WorkLifeBalance 1 'Bad' 2 'Good' 3 'Better' 4 'Best'
}
\source{
\link{https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(HR_Employee_Attrition)
}
\keyword{datasets}
