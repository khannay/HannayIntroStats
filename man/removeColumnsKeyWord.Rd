\name{removeColumnsKeyWord}
\alias{removeColumnsKeyWord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Columns from a data frame according to a keyword
}
\description{
This function will remove any columns in a data frame which contain the keyword given. If you flip this to remove=FALSE then it will retain only those columns with that keyword
}
\usage{
removeColumnsKeyWord(dataframe, keyword, remove=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
   The data frame to be changed
}
\item{keyword}{
   The keyword to be matched with grepl
}
\item{remove}{
   whether to remove the matching columns (logical)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the data frame with the columns removed (retained)
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
data("shot_logs_2014")
removeColumnsKeyWord(shot_logs_2014, "ID")


## The function is currently defined as
removeColumnsKeyWord<-function(df, keyword, remove=TRUE) {
  if(remove) {
    df<-df[,!grepl(tolower(keyword), tolower(names(df)))]
  } else {
    df<-df[,grepl(tolower(keyword), tolower(names(df)))]
  }

  return(df)
}


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
