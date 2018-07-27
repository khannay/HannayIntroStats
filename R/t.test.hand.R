#' T test calculator using summary statistics
#'
#' Function to form confidence intervals and perform t tests for data where we only have summary statistics. This function will perform inference for data sets where we only have the mean, sd and counts.
#'
#' @param m1 the mean of data set one (numeric value)
#' @param s1 the standard deviation of data set one (numeric value)
#' @param n1 the number of data points in data set one, the sample size (numeric value)
#' @param m2 the mean of data set two (numeric value)
#' @param s2 the standard deviation of data set two (numeric value)
#' @param n2 the number of data points in data set two, the sample size (numeric value)
#' @param mu a number indicating the true value of the mean (or difference in means if you are performing a two sample test).
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param conf.level confidence level of the interval.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#'
#' @return A list with class "htest" containing the following components:
#' \item{statistic}{the value of the t-statistic.}
#' \item{parameter}{the degrees of freedom for the t-statistic.}
#' \item{p.value}{the p-value for the test.}
#' \item{conf.int}{a confidence interval for the mean appropriate to the specified alternative hypothesis.}
#' \item{estimate}{the estimated mean or difference in means depending on whether it was a one-sample test or a two-sample test.}
#' \item{null.value}{the specified hypothesized value of the mean or mean difference depending on whether it was a one-sample test or a two-sample test.}
#' \item{alternative}{a character string describing the alternative hypothesis.}
#' \item{method}{a character string indicating what type of t-test was performed.}
#' \item{data.name}{a character string giving the name(s) of the data.}
#' @examples
#' t.test.hand(m1=10.0, s1=2.0, n1=100) #forn a 95% confidence interval
#' t.test.hand(m1=10.0, s1=2.0, n1=30, m2=9.0, s2=1.0, n2=40) #perform a two sided two sample t test
#' @export


t.test.hand <- function(m1,s1,n1, m2=NULL,s2=NULL,n2=NULL,mu=0, conf.level=0.95, var.equal=FALSE,  alternative=c("two.sided", "less", "greater"))
{

  alternative <- match.arg(alternative)

  if(!missing(mu) && (length(mu) != 1 || is.na(mu)))
    stop("'mu' must be a single number")
  if(!missing(conf.level) &&
     (length(conf.level) != 1 || !is.finite(conf.level) ||
      conf.level < 0 || conf.level > 1))
    stop("'conf.level' must be a single number between 0 and 1")

  if(is.null(m2)) {
    if(n1 < 2) stop("not enough 'x' observations")
    df <- n1-1
    v1<-s1^2
    sd <- sqrt(v1/n1)
    se<- sqrt( (s1^2/n1))
    if(sd < 10 *.Machine$double.eps * abs(m1))
      stop("data are essentially constant")
    tstat <- (m1-mu)/sd
    method <- "One Sample t-test"
    estimate <- setNames(m1, "mean of x")
  } else {

      if( var.equal==FALSE )
      {
        se <- sqrt( (s1^2/n1) + (s2^2/n2) )
        # welch-satterthwaite df
        df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
      } else {
        # pooled standard deviation, scaled by the sample sizes
        se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) )
        df <- n1+n2-2
      }
      tstat <- (m1-m2-mu)/se

      mx<- m1
      my <- m2
      method <- paste(if(!var.equal)"Welch", "Two Sample t-test")
      estimate <- c(mx,my)
      names(estimate) <- c("mean of x","mean of y")


  }#else

  #Find the p value using the alternative
  if (alternative=='two.sided') {
      p.value<-2*pt(-abs(tstat),df)
      alpha <- 1 - conf.level
      cint <- qt(1 - alpha/2, df)
      cint <- tstat + c(-cint, cint)
  }

  if (alternative=='greater'| alternative=='g') {
    p.value<-1-pt(tstat,df)
    cint <- c(tstat - qt(conf.level, df), Inf)
  }

  if (alternative=='less'| alternative=='l') {
    p.value<-pt(tstat,df)
    cint <- c(-Inf, tstat + qt(conf.level, df) )
  }

  cint <- mu + cint * se
  names(tstat) <- "t"
  names(df) <- "df"
  names(mu) <- if(!is.null(m2)) "difference in means" else "mean"
  attr(cint,"conf.level") <- conf.level
  rval <- list(statistic = tstat, parameter = df, p.value = p.value,
               conf.int = cint, estimate = estimate, null.value = mu,
               alternative = alternative,
               method = method, data.name='No names given')
  class(rval) <- "htest"
  return(rval)



  dat <- c(m1-m2, se, tstat, df, p.value, cint)
  names(dat) <- c("Difference of means (mean1-mean2)", "Std Error", "t", "df", "p-value", "cint")
  return(dat)
}
