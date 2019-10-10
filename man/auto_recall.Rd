\name{auto_recall}
\alias{auto_recall}
\docType{data}
\title{
Auto Recall Data set
}
\description{
The National Traffic and Motor Vehicle Safety Act (1966) gives the Department of Transportation’s National Highway Traffic Safety Administration (NHTSA) the authority to issue vehicle safety standards and to require manufacturers to recall vehicles that have safety-related defects or do not meet federal safety standards. More than 390 million cars, trucks, buses, recreational vehicles, motorcycles, and mopeds, 46 million tires, 66 million pieces of motor vehicle equipment, and 42 million child safety seats have been recalled to correct safety defects since 1967.

Manufacturers voluntarily initiate many of these recalls, while others are influenced by NHTSA investigations or ordered by NHTSA via the courts. If a safety defect is discovered, the manufacturer must notify NHTSA, vehicle or equipment owners, dealers, and distributors. The manufacturer is then required to remedy the problem at no charge to the owner. NHTSA is responsible for monitoring the manufacturer’s corrective action to ensure successful completion of the recall campaign.
}
\usage{data("auto_recall")}
\format{
  A data frame with 123837 observations on the following 15 variables.
  \describe{
    \item{\code{Record_ID}}{a numeric vector}
    \item{\code{NHTSA_Campaign}}{a character vector}
    \item{\code{Manufacturer_Campaign}}{a character vector}
    \item{\code{Vehicle_Make}}{a factor with many levels }
    \item{\code{Vehicle_Model}}{a factor with many  many levels }
    \item{\code{Manufacture_Start_Date}}{a numeric vector}
    \item{\code{Manufacture_End_Date}}{a numeric vector}
    \item{\code{Estimated_Units}}{a numeric vector}
    \item{\code{Recall_Initiative}}{a factor with levels \code{MFR} \code{ODI} \code{OVSC}}
    \item{\code{Recall_Manufacturer}}{a character vector}
    \item{\code{Recall_Notification_Date}}{a Date}
  }
}
\details{

}
\source{
https://www.kaggle.com/nhtsa/safety-recalls
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(auto_recall)
## maybe str(auto_recall) ; plot(auto_recall) ...
}
\keyword{datasets}
