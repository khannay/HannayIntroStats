#' Fix the column names for a data frame
#'
#' Will remove spaces and other issues from the names
#'
#' @param df the input df
#'
#' @return new df with names fixed
#' @examples
#' fixColumnNames(df)
#' @export


fixColumnNames <- function(df)  {
  names(df)<-stringr::str_replace_all(names(df), c(" " = "_" , "," = "_", "/"="_", "\\("="", "\\)"=""))
  return(df)
}
