\name{BlackFriday}
\alias{BlackFriday}
\docType{data}
\title{
Black Friday Sales
}
\description{
This data set contains over 500k observations of black friday sales in a retail store.
}
\usage{data("BlackFriday")}
\format{
  A data frame with 537577 observations on the following 12 variables.
  \describe{
    \item{\code{User_ID}}{a factor with 5891 levels giving the unique customer making the purchase}
    \item{\code{Gender}}{a character vector giving the gender of the customer}
    \item{\code{Age}}{a factor giving age range with levels \code{0-17} \code{18-25} \code{26-35} \code{36-45} \code{46-50} \code{51-55} \code{55+}}
    \item{\code{Occupation}}{a factor with levels \code{0} \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{10} \code{11} \code{12} \code{13} \code{14} \code{15} \code{16} \code{17} \code{18} \code{19} \code{20}}
    \item{\code{City_Category}}{a factor with levels \code{A} \code{B} \code{C}}
    \item{\code{Stay_In_Current_City_Years}}{a factor with levels \code{0} \code{1} \code{2} \code{3} \code{4+}}
    \item{\code{Marital_Status}}{a factor with levels \code{0} \code{1}}
    \item{\code{Product_Category_1}}{a numeric vector}
    \item{\code{Product_Category_2}}{a numeric vector}
    \item{\code{Product_Category_3}}{a numeric vector}
    \item{\code{Purchase}}{a numeric vector giving the dollars spent}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
\url{https://www.kaggle.com/mehdidag/black-friday}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(BlackFriday)
library(dplyr)
BlackFriday %>% group_by(User_ID) %>% summarise(total.spend=sum(Purchase)) %>% arrange(desc(total.spend)) %>% barplot()
}
\keyword{datasets}
