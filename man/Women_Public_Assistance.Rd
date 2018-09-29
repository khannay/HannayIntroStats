\name{Women_Public_Assistance}
\alias{Women_Public_Assistance}
\docType{data}
\title{
Women Public Assistance
}
\description{
This dataset focuses on public assistance in the United States with initial coverage of the WIC Program. The program is formally known as the Special Supplemental Nutrition Program for Women, Infants, and Children (WIC). The program allocates Federal and State funds to help low-income women and children up to age five who are at nutritional risk. Funds are used to provide supplemental foods, baby formula, health care, and nutrition education.
}
\usage{data("Women_Public_Assistance")}
\format{
  A data frame with 48 monthly observations on the following 58 variables (date, year, month, all states and the Total assistance across all states)
  \describe{
    \item{\code{Date.Recorded}}{a Date}
    \item{\code{Month}}{a factor with levels \code{01} \code{02} \code{03} \code{04} \code{05} \code{06} \code{07} \code{08} \code{09} \code{10} \code{11} \code{12}}
    \item{\code{Year}}{a factor with levels \code{2012} \code{2013} \code{2014} \code{2015} \code{2016}}
    \item{\code{Connecticut}}{a numeric vector giving the dollars of assistance provided that month same for all other states}
    \item{\code{Maine}}{a numeric vector}
    \item{\code{Massachusetts}}{a numeric vector}
    \item{\code{New.Hampshire}}{a numeric vector}
    \item{\code{New.York}}{a numeric vector}
    \item{\code{Rhode.Island}}{a numeric vector}
    \item{\code{Vermont}}{a numeric vector}
    \item{\code{Delaware}}{a numeric vector}
    \item{\code{District.of.Columbia}}{a numeric vector}
    \item{\code{Maryland}}{a numeric vector}
    \item{\code{New.Jersey}}{a numeric vector}
    \item{\code{Pennsylvania}}{a numeric vector}
    \item{\code{Puerto.Rico}}{a numeric vector}
    \item{\code{Virginia}}{a numeric vector}
    \item{\code{Virgin.Islands}}{a numeric vector}
    \item{\code{West.Virginia}}{a numeric vector}
    \item{\code{Alabama}}{a numeric vector}
    \item{\code{Florida}}{a numeric vector}
    \item{\code{Georgia}}{a numeric vector}
    \item{\code{Kentucky}}{a numeric vector}
    \item{\code{Mississippi}}{a numeric vector}
    \item{\code{North.Carolina}}{a numeric vector}
    \item{\code{South.Carolina}}{a numeric vector}
    \item{\code{Tennessee}}{a numeric vector}
    \item{\code{Illinois}}{a numeric vector}
    \item{\code{Indiana}}{a numeric vector}
    \item{\code{Michigan}}{a numeric vector}
    \item{\code{Minnesota}}{a numeric vector}
    \item{\code{Ohio}}{a numeric vector}
    \item{\code{Wisconsin}}{a numeric vector}
    \item{\code{Arkansas}}{a numeric vector}
    \item{\code{Louisiana}}{a numeric vector}
    \item{\code{New.Mexico}}{a numeric vector}
    \item{\code{Oklahoma}}{a numeric vector}
    \item{\code{Texas}}{a numeric vector}
    \item{\code{Colorado}}{a numeric vector}
    \item{\code{Iowa}}{a numeric vector}
    \item{\code{Kansas}}{a numeric vector}
    \item{\code{Missouri}}{a numeric vector}
    \item{\code{Montana}}{a numeric vector}
    \item{\code{Nebraska}}{a numeric vector}
    \item{\code{North.Dakota}}{a numeric vector}
    \item{\code{South.Dakota}}{a numeric vector}
    \item{\code{Utah}}{a numeric vector}
    \item{\code{Wyoming}}{a numeric vector}
    \item{\code{Alaska}}{a numeric vector}
    \item{\code{Arizona}}{a numeric vector}
    \item{\code{California}}{a numeric vector}
    \item{\code{Guam}}{a numeric vector}
    \item{\code{Hawaii}}{a numeric vector}
    \item{\code{Idaho}}{a numeric vector}
    \item{\code{Nevada}}{a numeric vector}
    \item{\code{Oregon}}{a numeric vector}
    \item{\code{Washington}}{a numeric vector}
    \item{\code{Total}}{a numeric vector giving the Total assistance provided that month for all states in the data set}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
The U.S. Census Bureau’s Small Area Income and Poverty Estimates program produces single-year estimates of income and poverty for all U.S. states and counties as well as estimates of school-age children in poverty for all 13,000+ school districts.

}
\references{
US Census Bureau https://www.census.gov/programs-surveys/saipe/data/datasets.All.html
}
\examples{
data(Women_Public_Assistance)
#Plot the total assistance provided for each month in the data set
plot(Women_Public_Assistance$Date.Recorded, Women_Public_Assistance$Total)
#Plot the assistance provided in Texas
plot(Women_Public_Assistance$Date.Recorded, Women_Public_Assistance$Texas)
}
\keyword{datasets}
