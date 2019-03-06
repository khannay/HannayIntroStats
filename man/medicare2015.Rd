\name{medicare2015}
\alias{medicare2015}
\docType{data}
\title{
Medicare Data Set from 2015
}
\description{
The Skilled Nursing Facility Utilization and Payment Public Use File (Skilled Nursing Facility PUF) provides information on services provided to Medicare beneficiaries residing in skilled nursing facilities. The Skilled Nursing Facility PUF contains information on utilization, payment (allowed amount, Medicare payment and standard payment), submitted charges and beneficiary demographic and chronic condition indicators
}
\usage{data("medicare2015")}
\format{
  A data frame with 15026 observations on the following 41 variables.
  \describe{
    \item{\code{Provider_ID}}{a numeric vector}
    \item{\code{Facility_Name}}{a character vector}
    \item{\code{Street_Address}}{a character vector}
    \item{\code{City}}{a character vector}
    \item{\code{State}}{a character vector}
    \item{\code{Zip_Code}}{a numeric vector}
    \item{\code{Total_Stays}}{a numeric vector}
    \item{\code{Distinct_Beneficiaries_Per_Provider}}{a numeric vector}
    \item{\code{Average_Length_of_Stay_Days}}{a numeric vector}
    \item{\code{Total_SNF_Charge_Amount}}{a numeric vector}
    \item{\code{Total_SNF_Medicare_Allowed_Amount}}{a numeric vector}
    \item{\code{Total_SNF_Medicare_Payment_Amount}}{a numeric vector}
    \item{\code{Total_SNF_Medicare_Standard_Payment_Amount}}{a numeric vector}
    \item{\code{Average_Age}}{a numeric vector}
    \item{\code{Male_Beneficiaries}}{a numeric vector}
    \item{\code{Female_Beneficiaries}}{a numeric vector}
    \item{\code{Nondual_Beneficiaries}}{a numeric vector}
    \item{\code{Dual_Beneficiaries}}{a numeric vector}
    \item{\code{White_Beneficiaries}}{a numeric vector}
    \item{\code{Black_Beneficiaries}}{a numeric vector}
    \item{\code{Asian_Pacific_Islander_Beneficiaries}}{a numeric vector}
    \item{\code{Hispanic_Beneficiaries}}{a numeric vector}
    \item{\code{American_Indian_or_Alaska_Native_Beneficiaries}}{a numeric vector}
    \item{\code{Other__Unknown_Beneficiaries}}{a numeric vector}
    \item{\code{Average_HCC_Score}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_Atrial_Fibrillation}}{a numeric vector}
    \item{\samp{Percent_of_Beneficiaries_with_Alzheimer's}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_Asthma}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_Cancer}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_CHF}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_Chronic_Kidney_Disease}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_COPD}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_Depression}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_Diabetes}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_Hyperlipidemia}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_Hypertension}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_IHD}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_Osteoporosis}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_RA_OA}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_Schizophrenia}}{a numeric vector}
    \item{\code{Percent_of_Beneficiaries_with_Stroke}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
https://www.kaggle.com/cms/medicare-skilled-nursing-facility-provider-reports
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(medicare2015)
}
\keyword{datasets}
