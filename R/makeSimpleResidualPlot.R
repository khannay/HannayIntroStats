



makeSimpleResidualPlot<-function(alpha=0, beta=1, x, y, main='', xlab='', ylab='') {

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
