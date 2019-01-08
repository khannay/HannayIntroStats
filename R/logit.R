#' Logit Function for Logistic Regression
#'
#' This function is used in logistic regression
#'
#' @param x input value
#'
#' @return logit(x)
#' @examples
#' logit(10.0)
#' @export


logit<- function(x) {log(x/(1-x));}
