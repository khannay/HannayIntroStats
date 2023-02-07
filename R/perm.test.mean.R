#' Function to do a permutation test for a differences in means
#'
#' This does a nonparametric permutation test for a difference in the means between the two samples
#'
#' @param x this is the first sample in the data set
#' @param y this is the second sample in the data set
#' @param resamples defaults to 10000 this is how many resamples to take of the data set. Can increase this to get a more exact p value
#'
#' @return the estimated p value
#' @examples
#' data(hkhw) #load the Hong Kong Heights and Weights
#' bootstrap.median.confidence(hkhw$Weight.lbs) #form the 95% confidence interval for the median weight of Hong Kong Children
#' @export

perm.test.mean <- function(x,y, resamples=10000) {

  n1=length(x)
  n2=length(y)

  data=c(x,y)

  test_stat=abs(mean(x)-mean(y))
  count=0

  total=length(data)

  for (i in 1:resamples) {
    re.data=sample(data)
    val=abs(mean(re.data[1:n1])-mean(re.data[n1:total]))
    if (val>test_stat) {
      count=count+1
    }
  }
  return(count/resamples)


}
