

fixColumnNames <- function(df)  {
  names(df)<-stringr::str_replace_all(names(df), c(" " = "_" , "," = "_", "/"="_", "\\("="", "\\)"=""))
  return(df)
}
