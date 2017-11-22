\name{hkhw}
\alias{hkhw}
\docType{data}
\title{
Heights and Weights of 200 Hong Kong Children
}
\description{
Human Height and Weight are mostly hereditable, but lifestyles, diet, health and environmental factors also play a role in determining individual's physical characteristics. The dataset below contains 200 synthetic records of human heights and weights of 18 years old children. These data were simulated based on a 1993 by a Growth Survey of 25,000 children from birth to 18 years of age recruited from Maternal and Child Health Centres (MCHC) and schools and were used to develop Hong Kong's current growth charts for weight, height, weight-for-age, weight-for-height and body mass index (BMI).

This data provdies a good example of data which is "mound shaped" in nature.
}
\usage{data("hkhw")}
\format{
  A data frame with 200 observations on the following 2 variables.
  \describe{
    \item{\code{Height.in}}{a numeric vector giving the Heights of the children in inches}
    \item{\code{Weight.lbs}}{a numeric vector giving the weights of the children in pounds}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
\link{http://wiki.stat.ucla.edu/socr/index.php/SOCR_Data_Dinov_020108_HeightsWeights}
}
\references{
The full data set can be downloaded from here: \link{http://socr.ucla.edu/docs/resources/SOCR_Data/SOCR_Data_Dinov_020108_HeightsWeights.html}
}
\examples{
data(hkhw)
hist(hkhw$Height.in)
qqnorm(hkhw$Height.in); qqline(hkhw$Height.in, col='red')
## maybe str(hkhw) ; plot(hkhw) ...
}
\keyword{datasets}
