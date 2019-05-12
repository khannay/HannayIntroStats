

makeOpenMathArray <- function(data) {
  x=toString(data[1])
  for (i in 2:length(data)) {
    x<-cat(x, ",", toString(data[i]))
  }
  print(x)
}
