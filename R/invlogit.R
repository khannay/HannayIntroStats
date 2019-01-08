#' Inverse Logit Function for Logistic Regression
#'
#' This function is used in logistic regression
#'
#' @param x input value (convert this to a probability)
#'
#' @return invlogit(x)
#' @examples
#' invlogit(10.0)
#' @export


invlogit<-function(x) {1/(1+exp(-1*x));}
