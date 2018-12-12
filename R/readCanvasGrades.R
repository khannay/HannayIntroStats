

readCanvasGrades <- function(filename) {
  headers = read.csv(filename, header = FALSE, nrows = 1, as.is = T)
  df = read.csv(filename, skip = 3, header = FALSE)
  colnames(df)= headers
  df=fixColumnNames(df)
  return(df)

}
