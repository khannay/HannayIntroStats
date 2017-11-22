
#' Function to drop all columns of a data frame which aren't Numeric
#'
#' This function is can be used to get a reduced data frame with only numeric columns. Useful when you want to perform clustering on a data frame with
#' the kmeans function which only takes numerical values.
#'
#' @param mydf this is the data frame you want to filter, this won't be changed at all by the function.
#' @return a data frame with the non numeric columns removed
#' @examples
#' data(Distel_Turtle_Data)
#' num.turtle<-<-filterNumericOnly(Distel_Turtle_Data)
#' @export

filterNumericOnly <- function(mydf) {
  nums<-sapply(mydf, is.numeric) #get a list of the columns which are numeric
  dfout<-mydf[,nums] #grab only the numeric columns
  return(dfout)
}
