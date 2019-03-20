\name{Hospitals}
\alias{Hospitals}
\docType{data}
\title{
Nationwide Hospitals Ratings
}
\description{
This data set contains ratings for over two thousand hospitals across the country.
}
\usage{data("Hospitals")}
\format{
  A data frame with 2301 observations on the following 18 variables.
  \describe{
    \item{\code{Provider.ID}}{a numeric vector giving a unique ID number to each hospital}
    \item{\code{Hospital.Name}}{a character vector giving the name of the hospital}
    \item{\code{City}}{a character vector giving the city of the hospital}
    \item{\code{State}}{a factor giving a two letter code for the state of the hospital}
    \item{\code{ZIP.Code}}{a numeric vector giving the zipcodes of the hospitals}
    \item{\code{County.Name}}{a factor with many levels giving the county}
    \item{\code{Hospital.Type}}{a factor with levels \code{Acute Care Hospitals} \code{Childrens} \code{Critical Access Hospitals}}
    \item{\code{Hospital.Ownership}}{a factor with levels \code{Government - Federal} \code{Government - Hospital District or Authority} \code{Government - Local} \code{Government - State} \code{Physician} \code{Proprietary} \code{Tribal} \code{Voluntary non-profit - Church} \code{Voluntary non-profit - Other} \code{Voluntary non-profit - Private}}
    \item{\code{Emergency.Services}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{Meets.criteria.for.meaningful.use.of.EHRs}}{a factor with levels \code{} \code{Not Available} \code{Y}}
    \item{\code{Hospital.overall.rating}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{Not Available}}
    \item{\code{Mortality.national.comparison}}{a factor with levels \code{Above the national average} \code{Below the national average} \code{Not Available} \code{Same as the national average}}
    \item{\code{Safety.of.care.national.comparison}}{a factor with levels \code{Above the national average} \code{Below the national average} \code{Not Available} \code{Same as the national average}}
    \item{\code{Readmission.national.comparison}}{a factor with levels \code{Above the national average} \code{Below the national average} \code{Not Available} \code{Same as the national average}}
    \item{\code{Patient.experience.national.comparison}}{a factor with levels \code{Above the national average} \code{Below the national average} \code{Not Available} \code{Same as the national average}}
    \item{\code{Effectiveness.of.care.national.comparison}}{a factor with levels \code{Above the national average} \code{Below the national average} \code{Not Available} \code{Same as the national average}}
    \item{\code{Timeliness.of.care.national.comparison}}{a factor with levels \code{Above the national average} \code{Below the national average} \code{Not Available} \code{Same as the national average}}
    \item{\code{Efficient.use.of.medical.imaging.national.comparison}}{a factor with levels \code{Above the national average} \code{Below the national average} \code{Not Available} \code{Same as the national average}}
  }
}
\details{
This file contains general information about all hospitals that have been registered with Medicare, including their addresses, type of hospital, and ownership structure. It also contains information about the quality of each hospital, in the form of an overall rating (1-5, where 5 is the best possible rating & 1 is the worst), and whether the hospital scored above, same as, or below the national average for a variety of measures.

This data was updated by CMS on July 25, 2017. CMS' overall rating includes 60 of the 100 measures for which data is collected & reported on Hospital Compare website (https://www.medicare.gov/hospitalcompare/search.html). Each of the measures have different collection/reporting dates, so it is impossible to specify exactly which time period this dataset covers. For more information about the timeframes for each measure, see: https://www.medicare.gov/hospitalcompare/Data/Data-Updated.html# For more information about the data itself, APIs and a variety of formats, see: https://data.medicare.gov/Hospital-Compare
}
\source{
Taken from https://www.kaggle.com/cms/hospital-general-information
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Hospitals)

}
\keyword{datasets}
