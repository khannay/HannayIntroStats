
#' Generated Non-Independent (correlated) Errors
#'
#' Makes a few lots which may be useful in determining whether linear regression is working for a data set.
#'
#' @param n defaults to 100 gives the length of the noise vector requested
#' @param lag fdefaults to 10 gives the lag time in the correlation
#' @param sigma defaults to 1, gives the standard deviation of the noise requested
#'
#' @return a vector of length n of correlated random samples
#' @examples
#' generateCorrelatedErrors(1000, sigma=2.0)
#' @export






generateCorrelatedErrors <- function(n=100, sigma=1.0, lag=10) {

  max.val=lag+n
  min.val=lag+1
  noise<-seq(1,max.val)

  noise[1:lag]<-rnorm(lag, sd=sigma) #make some random first entries up to the lag time
  for (i in lag:max.val) {
    low<-i-lag
    high<-i-1
    history<-mean(c(noise[low:high], rnorm(1, sd=sigma)))
    noise[i]<-history
  }
  noise2<-noise[min.val:max.val]
  return(noise2)
}
