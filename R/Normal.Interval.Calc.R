#' Normal Interval Calculator (a,b)
#'
#' Function to give the probability of a random sample from a Normal distribution lying in a given interval (a,b)
#' @param a no default gives the left side of the interval
#' @param b no default, gives the right side of the interval
#' @param mean defaults to zero, gives the mean of the normal distribution
#' @param sd defaults to one, gives the standard deviation of the normal distribution
#' @return the probability of a normal random variate with the given mean and sd lying in the given interval
#' @examples
#' Normal.Interval.Calc(-1,1) #empirical rule approx 68%
#' @export




Normal.Interval.Calc <- function(a,b, mean=0.0, sd=1.0) {

  assertthat::assert_that(a<b)
  return(pnorm(b, mean=mean, sd=sd)-pnorm(a, mean=mean, sd=sd))

}
