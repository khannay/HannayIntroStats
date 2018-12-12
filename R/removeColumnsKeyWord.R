


removeColumnsKeyWord<-function(df, keyword, remove=TRUE) {
  if(remove) {
    df<-df[,!grepl(tolower(keyword), tolower(names(df)))]
  } else {
    df<-df[,grepl(tolower(keyword), tolower(names(df)))]
  }

  return(df)
}
