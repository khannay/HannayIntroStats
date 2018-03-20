\name{wine_chem}
\alias{wine_chem}
\docType{data}
\title{
Wine Chemistry
}
\description{
These data are the results of a chemical analysis of
      wines grown in the same region in Italy but derived from three
      different cultivars.
      The analysis determined the quantities of 13 constituents
      found in each of the three types of wines
}
\usage{data("wine_chem")}
\format{
  A data frame with 178 observations on the following 14 variables.
  \describe{
    \item{\code{Class.of.Wine}}{a factor with levels \code{1} \code{2} \code{3} giving the class of the wine}
    \item{\code{Alcohol}}{a numeric vector}
    \item{\code{Malic.Acid}}{a numeric vector}
    \item{\code{Ash}}{a numeric vector}
    \item{\code{Alcalinty.ash}}{a numeric vector}
    \item{\code{Magnesium}}{a numeric vector}
    \item{\code{Phenols}}{a numeric vector}
    \item{\code{Flavanoids}}{a numeric vector}
    \item{\code{Nonflavanoids}}{a numeric vector}
    \item{\code{Proanthocyanins}}{a numeric vector}
    \item{\code{Color.Intensity}}{a numeric vector}
    \item{\code{Hue}}{a numeric vector}
    \item{\code{OD280.OD315.diluted.wines}}{a numeric vector}
    \item{\code{Proline}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Forina, M. et al, PARVUS - An Extendible Package for Data
       Exploration, Classification and Correlation. Institute of Pharmaceutical
       and Food Analysis and Technologies, Via Brigata Salerno,
       16147 Genoa, Italy.

       \url{https://archive.ics.uci.edu/ml/machine-learning-databases/wine/}
}
\references{
S. Aeberhard, D. Coomans and O. de Vel,
   Comparison of Classifiers in High Dimensional Settings,
   Tech. Rep. no. 92-02, (1992), Dept. of Computer Science and Dept. of
   Mathematics and Statistics, James Cook University of North Queensland.
   (Also submitted to Technometrics).
}
\examples{
data(wine_chem)
}
\keyword{datasets}
