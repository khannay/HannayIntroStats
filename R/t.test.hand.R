

t.test.hand <- function(m1,s1,n1, m2=NULL,s2=NULL,n2=NULL,mu=0, conf.level=0.95, var.equal=FALSE,  alternative=c("two.sided", "less", "greater"))
{
  if( var.equal==FALSE )
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) )
    df <- n1+n2-2
  }
  tstat <- (m1-m2-mu)/se

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

  print(cint)
  dat <- c(m1-m2, se, tstat, df, p.value, cint)
  names(dat) <- c("Difference of means (mean1-mean2)", "Std Error", "t", "df", "p-value", "cint")
  return(dat)
}
