#' Function to plot the residuals for a given simple OLS Problem
#'
#' This function makes a simple plot of the residuals as arrows versus a statistical model of the form y=alpha+beta*x. Will print the least
#' squares cost of the model.
#' @param alpha defaults to 0, y intercept of the linear model
#' @param beta defaults to 1, slope of the linear model
#' @param x no default, this is a vector of the x values for the data points
#' @param main, no default, the title of the graph
#' @param xlab, default to x , the x axis label of the graph
#' @param xlab, default to y , the y axis label of the graph
#' @return NULL (nothings returned)
#' @examples None
#' @export



makeSimpleResidualPlot<-function(alpha=0, beta=1, x, y, main='', xlab='x', ylab='y') {

  plot(x,y, main=main, xlab=xlab, ylab=ylab)
  abline(alpha,beta, col='red')
  cost=0.0
  for (i in 1:length(x)) {
    x1=x[i]
    y1=y[i]
    y2=alpha+beta*x[i]
    cost<-cost+(y2-y1)^2
    arrows(x1,y1,x1,y2, col='darkgreen', length=0.1)
  }
  print("Cost: ")
  print(cost)


}
