#' Grab Numeric
#'
#' This function returns a new data frame with only the numeric columns for the original data frame. This is especially useful for cluster analysis.
#'
#' @param mydf this is the data frame to strip of non numerical columns
#' @param drop defaults to NULL (none) use this if you want to drop one of more columns by name. Non numerical columns will be dropped automatically
#' @param drop.na.values defaults to TRUE, this will drop any rows which contain missing values. This is done AFTER removing the drop columns AND dropping non numerical columns
#' @return a data frame with all numerical columns
#' @examples
#' data("Bensonetal")
#' b2<-grabNumeric(Bensonetal) # removes non numerical columns
#' @export



grabNumeric <- function(mydf, drop=NULL, drop.na.values=TRUE) {
    mydf<-mydf[ , !(names(mydf) %in% drop)]
    nums<-sapply(mydf, is.numeric)
    ret.value<-mydf[,nums]
    if (drop.na.values==TRUE) {
      ret.value<-na.omit(ret.value)
    }
    return(ret.value)
}
