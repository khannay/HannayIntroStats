\name{chemical_children}
\alias{chemical_children}
\docType{data}
\title{
Chemical Children (Fake) Data Set
}
\description{
Lets say that we are investigating whether a large chemical company has been leaking toxic chemicals into the water supply of a town aptly called Chemical City. Let's say that the toxic chemical is known to stunt the growth of children. Therefore, we collect a random sample of heights of 3rd Grade children in Chemical City by measuring the heights of 42 children. The results of this fake survey are stored in the Heights column of this data frame.
}
\usage{data("chemical_children")}
\format{
  A data frame with 42 observations on the following variable.
  \describe{
    \item{\code{Heights}}{a numeric vector giving the height of the child in feet}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(chemical_children)
hist(chemical_children$Heights)
}
\keyword{datasets}
