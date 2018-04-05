#' Function to find a confidence interval for the median of a data set
#'
#' This function is useful for finding the two sided confidence interval for the MEDIAN of a data set, for which the t.test command cannot be used.
#'
#' @param mydata use this to pass in the data set (numeric vector) you wish to build the confidence interval for.
#' @param conf.level defaults to 0.95 can change this to 0.99 etc to change the confidence level required. Must be a number between 0 and 1
#' @param replicates defaults to 10,000 this is how many resamples to take of the data set. Can increase this to get more consistent confidence intervals.
#'
#' @return a vector giving the lower and upper bound of the requested confidence interval
#' @examples
#' data(hkhw) #load the Hong Kong Heights and Weights
#' bootstrap.median.confidence(hkhw$Weight.lbs) #form the 95% confidence interval for the median weight of Hong Kong Children
#' @export





bootstrap.median.confidence <- function(mydata, conf.level=0.95, replicates=10000) {
  #bootstap to get a confidence interval for the median of this data set
  numdatapoints<-length(mydata)
  resampled.values<-replicate(replicates, median(sample(mydata, numdatapoints, replace=TRUE)) )
  print(resampled.values)
  left.over=(1.0-conf.level)/2.0
  upper=1.0-left.over;
  lower=left.over
  ans<-quantile(resampled.values, c(lower, upper), names=FALSE)
  return(ans)
}
