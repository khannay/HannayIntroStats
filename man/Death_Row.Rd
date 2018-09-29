\name{Death_Row}
\alias{Death_Row}
\docType{data}
\title{
Texas Death Row Inmates
}
\description{
The Texas Department of Criminal Justice publishes various details, including the last words, of every inmate on death row they execute. This dataset includes information on the name, age, race, county, date, and last words of Texas death row inmates from 1982 to 2017.
}
\usage{data("Death_Row")}
\format{
  A data frame with 545 observations (executions) on the following 20 variables.
  \describe{
    \item{\code{Execution}}{a numeric vector}
    \item{\code{LastName}}{a character vector}
    \item{\code{FirstName}}{a character vector}
    \item{\code{TDCJNumber}}{a numeric vector}
    \item{\code{Age}}{a numeric vector}
    \item{\code{Race}}{a factor with levels \code{Black} \code{Hispanic} \code{Other} \code{White}}
    \item{\code{CountyOfConviction}}{a factor with many levels giving the Texas county of the conviction}
    \item{\code{AgeWhenReceived}}{a numeric vector}
    \item{\code{EducationLevel}}{a numeric vector}
    \item{\code{NativeCounty}}{a numeric vector}
    \item{\code{PreviousCrime}}{a numeric vector}
    \item{\code{Codefendants}}{a numeric vector}
    \item{\code{NumberVictim}}{a numeric vector}
    \item{\code{WhiteVictim}}{a numeric vector}
    \item{\code{HispanicVictim}}{a numeric vector}
    \item{\code{BlackVictim}}{a numeric vector}
    \item{\samp{VictimOther Races}}{a numeric vector}
    \item{\code{FemaleVictim}}{a numeric vector}
    \item{\code{MaleVictim}}{a numeric vector}
    \item{\code{LastStatement}}{a character vector}
  }
}
\details{
The following crimes are Capital Murder in Texas:

    murder of a peace officer or fireman who is acting in the lawful discharge of an official duty and who the person knows is a peace officer or fireman;

    murder during the commission or attempted commission of kidnapping, burglary, robbery, aggravated sexual assault, arson, obstruction or retaliation, or terroristic threat;

    murder for remuneration or promise of remuneration or employs another to commit murder for remuneration or promise of remuneration;

    murder during escape or attempted escape from a penal institution;

    murder, while incarcerated in a penal institution, of a correctional employee or with the intent to establish, maintain, or participate in a combination or in the profits of a combination;

    murder while incarcerated in a penal institution for a conviction of murder or capital murder;

    murder while incarcerated in a penal institution serving a life sentence or a 99 year sentence for a conviction of aggravated kidnapping, aggravated sexual assault, or aggravated robbery;

    murder of more than one person during the same criminal transaction or during different criminal transactions but the murders are committed pursuant to the same scheme or course of conduct;

    murder of an individual under ten years of age; or

    murder in retaliation for or on account of the service or status of the other person as a judge or justice of the supreme court, the court of criminal appeals, a court of appeals, a district court, a criminal district court, a constitutional county court, a statutory county court, a justice court, or a municipal court.

}
\source{
https://www.kaggle.com/ianmobbs/texas-death-row-executions-info-and-last-words
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Death_Row)
table(Death_Row$Race)
}
\keyword{datasets}
