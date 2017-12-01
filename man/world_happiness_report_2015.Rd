\name{world_happiness_report_2015}
\alias{world_happiness_report_2015}
\docType{data}
\title{
Happiness Score by Country and World Region
}
\description{
The World Happiness Report is a landmark survey of the state of global happiness.The World Happiness 2015, which ranks 158 countries by their happiness levels, was released at the United Nations at an event celebrating International Day of Happiness on March 20th. The report continues to gain global recognition as governments, organizations and civil society increasingly use happiness indicators to inform their policy-making decisions. Leading experts across fields – economics, psychology, survey analysis, national statistics, health, public policy and more – describe how measurements of well-being can be used effectively to assess the progress of nations. The reports review the state of happiness in the world today and show how the new science of happiness explains personal and national variations in happiness.
}
\usage{data("world_happiness_report_2015")}
\format{
  A data frame with 158 observations on the following 11 variables.
  \describe{
    \item{\code{Country}}{a character vector}
    \item{\code{Region}}{a factor with levels \code{Australia and New Zealand} \code{Central and Eastern Europe} \code{Eastern Asia} \code{Latin America and Caribbean} \code{Middle East and Northern Africa} \code{North America} \code{Southeastern Asia} \code{Southern Asia} \code{Sub-Saharan Africa} \code{Western Europe}}
    \item{\code{Happiness.Score}}{a numeric vector giving the overall happiness score}
    \item{\code{Standard.Error}}{a numeric vector giving the estimated standard error for the happiness score column}
    \item{\code{GDP.Per.Capita.Score}}{a numeric vector giving the economic rating for that country}
    \item{\code{Family.Score}}{a numeric vector, giving the family rating for that country}
    \item{\code{Life.Expectancy.Score}}{a numeric vector the health score for the country}
    \item{\code{Freedom.Score}}{a numeric vector giving the personal freedom score for that country}
    \item{\code{Gov.Trust.Score}}{a numeric vector the trust in the government score for that country}
    \item{\code{Generosity.Score}}{a numeric vector giving the generosity score for that country}
    \item{\code{Dystopia.Residual}}{a numeric vector, see below for a discussion of the distopia score for a country}
  }
}
\details{
The happiness scores and rankings use data from the Gallup World Poll. The scores are based on answers to the main life evaluation question asked in the poll. This question, known as the Cantril ladder, asks respondents to think of a ladder with the best possible life for them being a 10 and the worst possible life being a 0 and to rate their own current lives on that scale. The scores are from nationally representative samples for the years 2013-2016 and use the Gallup weights to make the estimates representative. The columns following the happiness score estimate the extent to which each of six factors – economic production, social support, life expectancy, freedom, absence of corruption, and generosity – contribute to making life evaluations higher in each country than they are in Dystopia, a hypothetical country that has values equal to the world’s lowest national averages for each of the six factors. They have no impact on the total score reported for each country, but they do explain why some countries rank higher than others.

Dystopia is an imaginary country that has the world’s least-happy people. The purpose in establishing Dystopia is to have a benchmark against which all countries can be favorably compared (no country performs more poorly than Dystopia) in terms of each of the six key variables, thus allowing each sub-bar to be of positive width. The lowest scores observed for the six key variables, therefore, characterize Dystopia. Since life would be very unpleasant in a country with the world’s lowest incomes, lowest life expectancy, lowest generosity, most corruption, least freedom and least social support, it is referred to as “Dystopia,” in contrast to Utopia.
}
\source{
Gallup World Poll.

}
\references{
\link{https://www.kaggle.com/unsdsn/world-happiness}
}
\examples{
data(world_happiness_report_2015)
}
\keyword{datasets}
