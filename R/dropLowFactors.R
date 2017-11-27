#' Function to filter out levels of factors with low counts from a data frame
#'
#' This will drop any row which has an entry in the factor.column number that appears less than threshold number of times in the data.
#' @param mydf no default this is the original data frame
#' @param factor.column defaults to 1, tells the function which column to consider, this should be a factor
#' @param threshold defaults to 30, this function will drop all rows with level of the factor.column that appears less than threshold number of times
#' @return new data frame with entries removed
#' @examples None
#' @export



dropLowFactors <- function(mydf, factor.column=1, threshold=30) {

  myt<-table(mydf[,factor.column])
  names.factor<-names(subset(myt, myt>=threshold))
  df.out<-droplevels(subset(mydf, mydf[,factor.column] %in% names.factor))
  return(df.out)
}
