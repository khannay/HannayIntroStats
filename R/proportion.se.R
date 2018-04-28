#' Compute the Standard Error for a Population Proportion
#'
#' This is a short-hand function to find the standard error for a standard error of
#' the population proportion calculation. se=sqrt(p*(1-p)/N)
#'
#' @param success is the number of successes in the sample
#' @param total is the TOTAL number of trials in the data set
#' @return the estimated standard error for estimator
#' @examples
#' proportion.se(10,25)
#' @export

proportion.se <- function(success, total) {

  p=success/total
  se=sqrt(p*(1-p)/total)
  return(se)
}
