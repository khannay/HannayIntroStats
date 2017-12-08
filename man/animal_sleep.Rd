\name{animal_sleep}
\alias{animal_sleep}
\docType{data}
\title{
Animal Sleep Data
}
\description{
Sleep Patterns of 83 Animal Species
}
\usage{data("animal_sleep")}
\format{
  A data frame with 83 observations on the following 11 variables.
  \describe{
    \item{\code{name}}{a factor with 83 levels giving the common name of the species}
    \item{\code{vore}}{a factor with levels \code{carni} \code{herbi} \code{insecti} \code{omni} giving the diet of the species}
    \item{\code{order}}{a factor with levels \code{Afrosoricida} \code{Artiodactyla} \code{Carnivora} \code{Cetacea} \code{Chiroptera} \code{Cingulata} \code{Didelphimorphia} \code{Diprotodontia} \code{Erinaceomorpha} \code{Hyracoidea} \code{Lagomorpha} \code{Monotremata} \code{Perissodactyla} \code{Pilosa} \code{Primates} \code{Proboscidea} \code{Rodentia} \code{Scandentia} \code{Soricomorpha} giving the scientific order of the species}
    \item{\code{conservation}}{a factor with levels \code{cd} \code{domesticated} \code{en} \code{lc} \code{nt} \code{vu} many missing values}
    \item{\code{sleep_total}}{a numeric vector giving the total amount of sleep in hours}
    \item{\code{sleep_rem}}{a numeric vector giving the total rem sleep in hours}
    \item{\code{sleep_cycle}}{a numeric vector the length of the sleep cycle in hours}
    \item{\code{awake}}{a numeric vector giving the amount of time spent awake in hours}
    \item{\code{brainwt}}{a numeric vector giving the weight of the species brain in kilograms (missing values)}
    \item{\code{bodywt}}{a numeric vector giving the body weight of the species in kg}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
data provided by Karen Svenson <Karen.Svenson@jax.org>
via
Gary Churchill <Gary.Churchill@jax.org>
Dan Gatti <Dan.Gatti@jax.org> and
The data collection was partially funded by P50 GM070683
}
\references{
\url{http://genomicsclass.github.io/book/pages/dplyr_tutorial.html}
}
\examples{
data(animal_sleep)
}
\keyword{datasets}
