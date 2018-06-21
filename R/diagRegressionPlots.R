
#' Regression Diagnostics
#'
#' Makes a few plots which may be useful in determining whether ordinary linear regression is working for a data set.
#'
#' @param regression.obj no default, is a the output from an lm command in R
#' @param cex defaults to 1, controls the size of the points in the scatter plots decrease if you have a large number of points
#' @examples
#' data("alligator")
#' lm.alligator=lm(lnLength~lnWeight, data=alligator)
#' summary(lm.alligator)
#' diagRegressionPlots(lm.alligator)
#' @export



diagRegressionPlots <- function(regression.obj, cex=1)
{
  par(mfrow=c(2,2))
  #Grab the names of the two variables
  var.names=attr(regression.obj$model, "names")

  x=regression.obj$model[,2]
  y=regression.obj$model[,1]


  qqnorm(as.vector(regression.obj$residuals), main="Normality check QQ Plot", cex=cex)
  qqline(as.vector(regression.obj$residuals), col='red')
  plot(x,regression.obj$residuals, main='Residual Plot', ylab='Residuals', xlab=var.names[2], cex=cex)
  hist(regression.obj$residuals, main='Histogram for the Residuals', xlab='Residual Value', col='lightblue')

  #plot the regression
  title=paste(c('Model Fit R^2=', round(summary(regression.obj)$r.squared,2)), collapse=" ")
  plot(x,y, main=title, xlab=var.names[1], ylab=var.names[2], cex=cex)
  abline(regression.obj, col='red')
  par(mfrow=c(1,1))
}
