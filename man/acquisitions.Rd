\name{acquisitions}
\alias{acquisitions}
\docType{data}
\title{
Company Acquisitions Data - Top 7 Companies
}
\description{
This dataset contains the list of acquisitions made by the following companies:

    Google
    Microsoft
    Facebook
    Apple
    Yahoo
    Twitter
    IBM

}
\usage{data("acquisitions")}
\format{
  A data frame with 864 observations on the following 6 variables.
  \describe{
    \item{\code{AcquisitionMonth}}{a factor with levels \code{April} \code{August} \code{December} \code{February} \code{Feburary} \code{January} \code{July} \code{June} \code{March} \code{May} \code{November} \code{October} \code{September}}
    \item{\code{AcquisitionYear}}{a numeric vector giving the year of the acquisitions}
    \item{\code{Company}}{a character vector giving the name of the company acquired}
    \item{\code{Business}}{a character vector giving the type of business acquired}
    \item{\code{Country}}{a factor giving the country code of the company which was acquired}
    \item{\code{ParentCompany}}{a factor with levels \code{Apple} \code{Facebook} \code{Google} \code{IBM} \code{Microsoft} \code{Twitter} \code{Yahoo}}
  giving the name of the company which performed the acquisition}
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Source of this dataset is Wikipedia

    https://en.wikipedia.org/wiki/List_of_mergers_and_acquisitions_by_Alphabet (/Apple/Facebook/IBM/Yahoo!/Twitter/Microsoft)

Also, downloaded and proceesed from https://www.kaggle.com/shivamb/company-acquisitions-7-top-companies
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(acquisitions)
barplot(table(acquisitions$ParentCompany), las=2)
}
\keyword{datasets}
