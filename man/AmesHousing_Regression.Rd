\name{AmesHousing_Regression}
\alias{AmesHousing_Regression}
\docType{data}
\title{
Ames Housing Data Set: Simple Regression Set Up
}
\description{
Data set contains information from the Ames Assessor’s Office used in computing assessed values for individual residential properties sold in Ames, IA from 2006 to 2010.
}
\usage{data("AmesHousing_Regression")}
\format{
  A data frame with 2930 observations on the following 4 variables.
  \describe{
    \item{\code{Square.Feet.log10}}{a numeric vector giving the log base 10 of the square footage of the house}
    \item{\code{SalePrice.log10}}{a numeric vector giving the log base 10 of the sales price of the house}
    \item{\code{Neighborhood}}{Neighborhood of the house, encoded as a factor with levels \code{Blmngtn} \code{Blueste} \code{BrDale} \code{BrkSide} \code{ClearCr} \code{CollgCr} \code{Crawfor} \code{Edwards} \code{Gilbert} \code{Greens} \code{GrnHill} \code{IDOTRR} \code{Landmrk} \code{MeadowV} \code{Mitchel} \code{NAmes} \code{NoRidge} \code{NPkVill} \code{NridgHt} \code{NWAmes} \code{OldTown} \code{Sawyer} \code{SawyerW} \code{Somerst} \code{StoneBr} \code{SWISU} \code{Timber} \code{Veenker}}
    \item{\code{Bathroom}}{a numeric vector giving the number of bathrooms in the house, including half-baths (no shower)}
  }
}
\details{
This data set was constructed for the purpose of an end of semester project for an undergraduate regression course. The original data (obtained directly from the Ames Assessor’s Office) is used for tax assessment purposes but lends itself directly to the prediction of home selling prices. The type of information contained in the data is similar to what a typical home buyer would want to know before making a purchase and students should find most variables straightforward and understandable.

PEDAGOGICAL NOTES:
Instructors unfamiliar with multiple regression may wish to use this data set in conjunction with an earlier JSE paper that reviews most of the major issues found in regression modeling:

Kuiper , S. (2008), “Introduction to Multiple Regression: How Much Is Your Car Worth?”, Journal of Statistics Education Volume 16, Number 3 (2008).

Outside of the general issues associated with multiple regression discussed in this article, this particular data set offers several opportunities to discuss how the purpose of a model might affect the type of modeling done. User of this data may also want to review another JSE article related directly to real estate pricing:

Pardoe , I. (2008), “Modeling home prices using realtor data”, Journal of Statistics Education Volume 16, Number 2 (2008).

One issue is in regards to homoscedasticity and assumption violations. The graph included in the article appears to indicate heteroscedasticity with variation increasing with sale price and this problem is evident in many simple home pricing models that focus only on house and lot sizes. Though this violation can be alleviated by transforming the response variable (sale price), the resulting equation yields difficult to interpret fitted values (selling price in log or square root dollars). This situation gives the instructor the opportunity to talk about the costs (biased estimators, incorrect statistical tests, etc.) and benefits (ease of use) of not correcting this assumption violation. If the purpose in building the model is simply to allow a typical buyer or real estate agent to sit down and estimate the selling price of a house, such transformations may be unnecessary or inappropriate for the task at hand. This issue could also open into a discussion on the contrasts and comparisons between data mining, predictive models, and formal statistical inference.

A second issue closely related to the intended use of the model, is the handling of outliers and unusual observations. In general, I instruct my students to never throw away data points simply because they do not match a priori expectations (or other data points). I strongly make this point in the situation where data are being analyzed for research purposes that will be shared with a larger audience. Alternatively, if the purpose is to once again create a common use model to estimate a “typical” sale, it is in the modeler’s best interest to remove any observations that do not seem typical (such as foreclosures or family sales).

}
\source{
Ames, Iowa Assessor’s Office
}
\references{
Data set was mined and formatted by:
Dean De Cock, Truman State University
}
\examples{
data(AmesHousing_Regression)
}
\keyword{datasets}
