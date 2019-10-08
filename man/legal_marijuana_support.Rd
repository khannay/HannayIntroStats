\name{legal_marijuana_support}
\alias{legal_marijuana_support}
\docType{data}
\title{
Gallup Polls for Support for Legalizing Marijuana in the US
}
\description{
Data collected over the last fifty years on support for legal marijuana use in the United States.
}
\usage{data("legal_marijuana_support")}
\format{
  A data frame with 20 observations on the following 9 variables.
  \describe{
    \item{\code{Year}}{a numeric vector}
    \item{\code{Month}}{a factor with levels \code{Apr} \code{Aug} \code{Jan} \code{Jun} \code{Mar} \code{May} \code{Nov} \code{Oct}}
    \item{\code{Asked_half_sample}}{a factor with levels \code{no} \code{yes}}
    \item{\code{Yes_Legal}}{a numeric vector}
    \item{\code{No_Illegal}}{a numeric vector}
    \item{\code{No_Opinion}}{a numeric vector}
    \item{\code{Percent_Yes}}{a numeric vector}
    \item{\code{Percent_No}}{a numeric vector}
    \item{\code{Percent_No_Opinion}}{a numeric vector}
  }
}
\details{
Results are based on telephone interviews conducted October 5-9, 2016 with a random sample of –1,017—adults, aged 18+, living in all 50 U.S. states and the District of Columbia. For results based on this sample of national adults, the margin of sampling error is ±4 percentage points at the 95% confidence level.
For results based on the sample of –520—national adults in Form A, the margin of sampling error is ±5 percentage points.
For results based on the sample of –497—national adults in Form B, the margins of sampling error is ±5 percentage points.
Interviews are conducted with respondents on landline telephones and cellular phones, with interviews conducted in Spanish for respondents who are primarily Spanish-speaking. Each sample of national adults includes a minimum quota of 60\% cell phone respondents and 40\% landline respondents, with additional
minimum quotas by region. Landline and cell phone telephone numbers are selected using random digit dial methods. Landline respondents are chosen at random within each household on the basis of which member has the next birthday.
Samples are weighted to correct for unequal selection probability, non-response, and double coverage of landline and cell users in the two sampling frames. They are also weighted to match the national demographics
of gender, age, race, Hispanic ethnicity, education, region, population density, and phone status (cell phoneonly/landline only/both and cell phone mostly). Demographic weighting targets are based on the March 2015
Current Population Survey figures for the aged 18 and older U.S. population. Phone status targets are based on the July-December 2015 National Health Interview Survey. Population density targets are based on the 2010 census. All reported margins of sampling error include the computed design effects for weighting. In addition to sampling error, question wording and practical difficulties in conducting surveys can introduce error or bias into the findings of public opinion polls.
}
\source{
http://www.gallup.com/file/poll/196556/161019LegalMarijuana.pdf
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(legal_marijuana_support)
## maybe str(legal_marijuana_support) ; plot(legal_marijuana_support) ...
}
\keyword{datasets}
