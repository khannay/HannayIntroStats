\name{Enrollment}
\alias{Enrollment}
\docType{data}
\title{
Schreiner University Enrollment
}
\description{
This data set contains class information about Schreiner University for the semesters Fall 2017-Fall 2018.
}
\usage{data("Enrollment")}
\format{
  A data frame with 1390 classes on the following 16 variables.
  \describe{
    \item{\samp{Course code}}{a character vector}
    \item{\code{Name}}{a character vector}
    \item{\code{Faculty}}{a factor with many levels giving the name of teacher}
    \item{\code{Status}}{a factor with levels \code{Full} \code{Open} tells if the class was closed at the end of registration.}
    \item{\code{Schedule}}{a character giving the meeting times}
    \item{\code{Credits}}{a numeric vector giving the number of credits for the class (hours)}
    \item{\code{Department}}{a factor with levels \code{ACCT} \code{ART} \code{BIOC} \code{BIOL} \code{BSAD} \code{CHEM} \code{CHIN} \code{CIT} \code{COMM} \code{DESN} \code{EDUC} \code{ENGL} \code{ENGR} \code{EXSI} \code{FIN} \code{FREN} \code{HIST} \code{IDST} \code{LANG} \code{MATH} \code{MGMT} \code{MILT} \code{MKTG} \code{MUSC} \code{NURS} \code{PBHL} \code{PHIL} \code{PHYS} \code{POLS} \code{PSYC} \code{RDNG} \code{RELI} \code{RLES} \code{SOCI} \code{SPAN} \code{SPED} \code{THRE} \code{TXST} \code{CCCM} \code{CCEX} \code{CCMG} \code{CCMT} \code{CCMU} \code{CCPH} \code{CCPO} \code{CCPS} \code{CCRE} \code{CCSC} \code{GERM}}
    \item{\code{Enroll.Cap}}{a numeric vector giving the max enrollment for the class}
    \item{\code{Open.Seats}}{a numeric vector giving the number of open seats in the class}
    \item{\code{Course.Num}}{a numeric vector}
    \item{\code{Section.Num}}{a numeric vector many of these are missing}
    \item{\code{Students}}{a numeric vector giving the number of students in the class}
    \item{\code{Semester}}{a factor with levels \code{Fall} \code{Spring} giving the semester}
    \item{\code{Year}}{a numeric vector giving the year the class was taught}
    \item{\code{Small.Class.Less.Than.8}}{a numeric vector 1 if the class had less than 8 students}
    \item{\code{Delivery}}{a character vector online versus FTF for face to face}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Schreiner University Course Catalog
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Enrollment)
}
\keyword{datasets}
