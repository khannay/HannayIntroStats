\name{student_math_grades}
\alias{student_math_grades}
\docType{data}
\title{
Student Mathematics Grades in Secondary School
}
\description{
The data were obtained in a survey of students math in secondary school. It contains a lot of interesting social, gender and study information about students.
}
\usage{data("student_math_grades")}
\format{
  A data frame with 395 observations on the following 33 variables.
  \describe{
    \item{\code{school}}{a factor with levels \code{GP} \code{MS}}
    \item{\code{sex}}{a factor with levels \code{F} \code{M}}
    \item{\code{age}}{a numeric vector}
    \item{\code{address}}{a factor with levels \code{R} \code{U}}
    \item{\code{famsize}}{a factor with levels \code{GT3} \code{LE3}}
    \item{\code{Pstatus}}{a factor with levels \code{A} \code{T}}
    \item{\code{Medu}}{a numeric vector}
    \item{\code{Fedu}}{a numeric vector}
    \item{\code{Mjob}}{a factor with levels \code{at_home} \code{health} \code{other} \code{services} \code{teacher}}
    \item{\code{Fjob}}{a factor with levels \code{at_home} \code{health} \code{other} \code{services} \code{teacher}}
    \item{\code{reason}}{a factor with levels \code{course} \code{home} \code{other} \code{reputation}}
    \item{\code{guardian}}{a factor with levels \code{father} \code{mother} \code{other}}
    \item{\code{traveltime}}{a numeric vector}
    \item{\code{studytime}}{a numeric vector}
    \item{\code{failures}}{a numeric vector}
    \item{\code{schoolsup}}{a factor with levels \code{no} \code{yes}}
    \item{\code{famsup}}{a factor with levels \code{no} \code{yes}}
    \item{\code{paid}}{a factor with levels \code{no} \code{yes}}
    \item{\code{activities}}{a factor with levels \code{no} \code{yes}}
    \item{\code{nursery}}{a factor with levels \code{no} \code{yes}}
    \item{\code{higher}}{a factor with levels \code{no} \code{yes}}
    \item{\code{internet}}{a factor with levels \code{no} \code{yes}}
    \item{\code{romantic}}{a factor with levels \code{no} \code{yes}}
    \item{\code{famrel}}{a numeric vector}
    \item{\code{freetime}}{a numeric vector}
    \item{\code{goout}}{a numeric vector}
    \item{\code{Dalc}}{a numeric vector}
    \item{\code{Walc}}{a numeric vector}
    \item{\code{health}}{a numeric vector}
    \item{\code{absences}}{a numeric vector}
    \item{\code{G1}}{a numeric vector}
    \item{\code{G2}}{a numeric vector}
    \item{\code{G3}}{a numeric vector}
  }
}
\details{

    school - student's school (binary: 'GP' - Gabriel Pereira or 'MS' - Mousinho da Silveira)

    sex - student's sex (binary: 'F' - female or 'M' - male)

    age - student's age (numeric: from 15 to 22)

    address - student's home address type (binary: 'U' - urban or 'R' - rural)

    famsize - family size (binary: 'LE3' - less or equal to 3 or 'GT3' - greater than 3)

    Pstatus - parent's cohabitation status (binary: 'T' - living together or 'A' - apart)

    Medu - mother's education (numeric: 0 - none, 1 - primary education (4th grade), 2 – 5th to 9th grade, 3 – secondary education or 4 – higher education)

    Fedu - father's education (numeric: 0 - none, 1 - primary education (4th grade), 2 – 5th to 9th grade, 3 – secondary education or 4 – higher education)

    Mjob - mother's job (nominal: 'teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other')

    Fjob - father's job (nominal: 'teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other')

    reason - reason to choose this school (nominal: close to 'home', school 'reputation', 'course' preference or 'other')

    guardian - student's guardian (nominal: 'mother', 'father' or 'other')

    traveltime - home to school travel time (numeric: 1 - <15 min., 2 - 15 to 30 min., 3 - 30 min. to 1 hour, or 4 - >1 hour)

    studytime - weekly study time (numeric: 1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours)

    failures - number of past class failures (numeric: n if 1<=n<3, else 4)

    schoolsup - extra educational support (binary: yes or no)

    famsup - family educational support (binary: yes or no)

    paid - extra paid classes within the course subject (Math or Portuguese) (binary: yes or no)

    activities - extra-curricular activities (binary: yes or no)

    nursery - attended nursery school (binary: yes or no)

    higher - wants to take higher education (binary: yes or no)

    internet - Internet access at home (binary: yes or no)

    romantic - with a romantic relationship (binary: yes or no)

    famrel - quality of family relationships (numeric: from 1 - very bad to 5 - excellent)

    freetime - free time after school (numeric: from 1 - very low to 5 - very high)

    goout - going out with friends (numeric: from 1 - very low to 5 - very high)

    Dalc - workday alcohol consumption (numeric: from 1 - very low to 5 - very high)

    Walc - weekend alcohol consumption (numeric: from 1 - very low to 5 - very high)

    health - current health status (numeric: from 1 - very bad to 5 - very good)

    absences - number of school absences (numeric: from 0 to 93)

    G1 - first period grade (numeric: from 0 to 20)

    G2 - second period grade (numeric: from 0 to 20)

    G3 - final grade (numeric: from 0 to 20, output target)

}
\source{
P. Cortez and A. Silva. Using Data Mining to Predict Secondary School Student Performance. In A. Brito and J. Teixeira Eds., Proceedings of 5th FUture BUsiness TEChnology Conference (FUBUTEC 2008) pp. 5-12, Porto, Portugal, April, 2008, EUROSIS, ISBN 978-9077381-39-7.

Fabio Pagnotta, Hossain Mohammad Amran. Email:fabio.pagnotta@studenti.unicam.it, mohammadamra.hossain '@' studenti.unicam.it University Of Camerino
}
\references{
\url{https://www.kaggle.com/uciml/student-alcohol-consumption}
}
\examples{
data(student_math_grades)
boxplot(student_math_grades$G1~student_math_grades$sex)
}
\keyword{datasets}
