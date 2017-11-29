
#' Generated Random Numbers From Bimodal Normal Distribution
#'
#' Generates random numbers from two normal distributions
#'
#' @param n no default is the number of random variates requested
#' @param mu1 default 1.0 is the mean of the first normal dist
#' @param mu2 default -1.0 is the mean of the second normal dist
#' @param sigma1 default 0.5 is the standard deviation of the first normal dist
#' @param sigma2 default 0.5 is the standard deviation of the second normal dist
#' @param p1 default 0.5 is the weight of the first normal distibution, p2 is 1.0-p1
#'
#' @return a vector of length n of random samples from the bimodal distribution
#' @examples
#' rbimodalNormal(100)
#' @export


rbimodalNormal <- function (n,mu1=1.0, mu2=-1.0, sigma1=0.5, sigma2=0.5, p1=0.5) {
 res<-seq(1,n)
 p2<-1.0-p1;

 for (i in 1:n) {
   x=rnorm(1, mean=mu1, sd=sigma1)
   y=rnorm(1, mean=mu2, sd=sigma2)
   res[i]<-sample(c(x,y), size=1, prob=c(p1,p2))
 }
 return(res)

}
