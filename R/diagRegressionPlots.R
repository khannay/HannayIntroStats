
#' Regression Diagnostics
#'
#' Makes a few lots which may be useful in determining whether linear regression is working for a data set.
#'
#' @param regression.obj no default, is a the output from an lm command in R
#' @param x, no default gives the x values used in the regression used to make a data plot
#' @param y, no default gives the y values used in the regression used to make a data plot
#' @examples
#' data("AmesHousing_Regression")
#' lm.res<-lm(SalePrice.log10~Square.Feet.log10, data=AmesHousing_Regression)
#' diagRegressionPlots(lm.res)
#' @export



diagRegressionPlots <- function(regression.obj, x, y) {
  par(mfrow=c(2,2))
  qqnorm(as.vector(regression.obj$residuals), main="Normality check for Residuals QQ Plot")
  qqline(as.vector(regression.obj$residuals), col='red')
  plot(regression.obj$residuals, main='Equal Variance Check for Residuals')
  hist(regression.obj$residuals, main='Histogram for the Residuals', xlab='Residual Value', col='lightblue')
  #plot the regression
  title=paste(c('Model Fit to the Data R Square=', round(summary(regression.obj)$r.squared,2)), collapse=" ")
  plot(x,y, main=title)
  abline(regression.obj, col='red')
}
