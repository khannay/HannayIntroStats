\name{fixColumnNames}
\alias{fixColumnNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
fixColumnNames
}
\description{
This is a simple function to remove spaces and other annoyances from column names in a data frame
}
\usage{
fixColumnNames(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Input data frame
}
}
\details{
names(df)<-stringr::str_replace_all(names(df), c(" " = "." , "," = ".", "/"="."))
}
\value{
dataframe with the column names fixed
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Kevin Hannay
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
