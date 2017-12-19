\name{Grocery_Data}
\alias{Grocery_Data}
\docType{data}
\title{ Grocery Store Data }
\description{
In this competition, you will be predicting the unit sales for thousands of items sold at different Favorita stores located in Ecuador. The training data includes dates, store and item information, whether that item was being promoted, as well as the unit sales.
}
\usage{data("Grocery_Data")}
\format{
  A data frame with 125497 observations on the following 14 variables.
  \describe{
    \item{\code{Item.Number}}{a numeric vector}
    \item{\code{Item.Sales}}{a numeric vector}
    \item{\code{Item.Promotion}}{a factor with levels \code{False} \code{True}}
    \item{\code{Item.Family}}{a factor with levels \code{AUTOMOTIVE} \code{BABY CARE} \code{BEAUTY} \code{BEVERAGES} \code{BREAD/BAKERY} \code{CELEBRATION} \code{CLEANING} \code{DAIRY} \code{DELI} \code{EGGS} \code{FROZEN FOODS} \code{GROCERY I} \code{GROCERY II} \code{HARDWARE} \code{HOME AND KITCHEN I} \code{HOME AND KITCHEN II} \code{HOME APPLIANCES} \code{HOME CARE} \code{LADIESWEAR} \code{LAWN AND GARDEN} \code{LINGERIE} \code{LIQUOR,WINE,BEER} \code{MAGAZINES} \code{MEATS} \code{PERSONAL CARE} \code{PET SUPPLIES} \code{PLAYERS AND ELECTRONICS} \code{POULTRY} \code{PREPARED FOODS} \code{PRODUCE} \code{SCHOOL AND OFFICE SUPPLIES} \code{SEAFOOD}}
    \item{\code{Item.Perishable}}{a factor with levels \code{0} \code{1}}
    \item{\code{Date.DayOfWeek}}{a numeric vector}
    \item{\code{Date.Month}}{a numeric vector}
    \item{\code{Date.Day}}{a numeric vector}
    \item{\code{Date.Year}}{a numeric vector}
    \item{\code{Store.Number}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{10} \code{11} \code{12} \code{13} \code{14} \code{15} \code{16} \code{17} \code{18} \code{19} \code{20} \code{21} \code{22} \code{23} \code{24} \code{25} \code{26} \code{27} \code{28} \code{29} \code{30} \code{31} \code{32} \code{33} \code{34} \code{35} \code{36} \code{37} \code{38} \code{39} \code{40} \code{41} \code{42} \code{43} \code{44} \code{45} \code{46} \code{47} \code{48} \code{49} \code{50} \code{51} \code{52} \code{53} \code{54}}
    \item{\code{Store.City}}{a factor with levels \code{Ambato} \code{Babahoyo} \code{Cayambe} \code{Cuenca} \code{Daule} \code{El Carmen} \code{Esmeraldas} \code{Guaranda} \code{Guayaquil} \code{Ibarra} \code{Latacunga} \code{Libertad} \code{Loja} \code{Machala} \code{Manta} \code{Playas} \code{Puyo} \code{Quevedo} \code{Quito} \code{Riobamba} \code{Salinas} \code{Santo Domingo}}
    \item{\code{Store.State}}{a factor with levels \code{Azuay} \code{Bolivar} \code{Chimborazo} \code{Cotopaxi} \code{El Oro} \code{Esmeraldas} \code{Guayas} \code{Imbabura} \code{Loja} \code{Los Rios} \code{Manabi} \code{Pastaza} \code{Pichincha} \code{Santa Elena} \code{Santo Domingo de los Tsachilas} \code{Tungurahua}}
    \item{\code{Store.Type}}{a factor with levels \code{A} \code{B} \code{C} \code{D} \code{E}}
    \item{\code{Store.Cluster}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{10} \code{11} \code{12} \code{13} \code{14} \code{15} \code{16} \code{17}}
  }
}
\details{
Wages in the public sector are paid every two weeks on the 15 th and on the last day of the month. Supermarket sales could be affected by this. A magnitude 7.8 earthquake struck Ecuador on April 16, 2016. People rallied in relief efforts donating water and other first need products which greatly affected supermarket sales for several weeks after the earthquake.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Grocery_Data)
}
\keyword{datasets}
