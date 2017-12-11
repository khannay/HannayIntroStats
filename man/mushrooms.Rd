\name{mushrooms}
\alias{mushrooms}
\docType{data}
\title{
Poisonous Mushrooms Statistics
}
\description{
Mushroom hunting has grown in popularity in recent years. However, this is a dangerous process as many of the mushrooms found in the wild are poisonous and only a mushroom expert (not you) should eat wild mushrooms. This data set has 22 categorical characteristics of mushrooms and the class column tells you whether the mushroom is poisonous or edible.
}
\usage{data("mushrooms")}
\format{
  A data frame with 8124 observations on the following 23 variables.
  \describe{
    \item{\code{class}}{a factor with levels \code{e} \code{p}}
    \item{\code{cap.shape}}{a factor with levels \code{b} \code{c} \code{f} \code{k} \code{s} \code{x}}
    \item{\code{cap.surface}}{a factor with levels \code{f} \code{g} \code{s} \code{y}}
    \item{\code{cap.color}}{a factor with levels \code{b} \code{c} \code{e} \code{g} \code{n} \code{p} \code{r} \code{u} \code{w} \code{y}}
    \item{\code{bruises}}{a factor with levels \code{f} \code{t}}
    \item{\code{odor}}{a factor with levels \code{a} \code{c} \code{f} \code{l} \code{m} \code{n} \code{p} \code{s} \code{y}}
    \item{\code{gill.attachment}}{a factor with levels \code{a} \code{f}}
    \item{\code{gill.spacing}}{a factor with levels \code{c} \code{w}}
    \item{\code{gill.size}}{a factor with levels \code{b} \code{n}}
    \item{\code{gill.color}}{a factor with levels \code{b} \code{e} \code{g} \code{h} \code{k} \code{n} \code{o} \code{p} \code{r} \code{u} \code{w} \code{y}}
    \item{\code{stalk.shape}}{a factor with levels \code{e} \code{t}}
    \item{\code{stalk.root}}{a factor with levels \code{?} \code{b} \code{c} \code{e} \code{r}}
    \item{\code{stalk.surface.above.ring}}{a factor with levels \code{f} \code{k} \code{s} \code{y}}
    \item{\code{stalk.surface.below.ring}}{a factor with levels \code{f} \code{k} \code{s} \code{y}}
    \item{\code{stalk.color.above.ring}}{a factor with levels \code{b} \code{c} \code{e} \code{g} \code{n} \code{o} \code{p} \code{w} \code{y}}
    \item{\code{stalk.color.below.ring}}{a factor with levels \code{b} \code{c} \code{e} \code{g} \code{n} \code{o} \code{p} \code{w} \code{y}}
    \item{\code{veil.type}}{a factor with levels \code{p}}
    \item{\code{veil.color}}{a factor with levels \code{n} \code{o} \code{w} \code{y}}
    \item{\code{ring.number}}{a factor with levels \code{n} \code{o} \code{t}}
    \item{\code{ring.type}}{a factor with levels \code{e} \code{f} \code{l} \code{n} \code{p}}
    \item{\code{spore.print.color}}{a factor with levels \code{b} \code{h} \code{k} \code{n} \code{o} \code{r} \code{u} \code{w} \code{y}}
    \item{\code{population}}{a factor with levels \code{a} \code{c} \code{n} \code{s} \code{v} \code{y}}
    \item{\code{habitat}}{a factor with levels \code{d} \code{g} \code{l} \code{m} \code{p} \code{u} \code{w}}
  }
}
\details{
Attribute Information: (classes: edible=e, poisonous=p)

    cap-shape: bell=b,conical=c,convex=x,flat=f, knobbed=k,sunken=s

    cap-surface: fibrous=f,grooves=g,scaly=y,smooth=s

    cap-color: brown=n,buff=b,cinnamon=c,gray=g,green=r,pink=p,purple=u,red=e,white=w,yellow=y

    bruises: bruises=t,no=f

    odor: almond=a,anise=l,creosote=c,fishy=y,foul=f,musty=m,none=n,pungent=p,spicy=s

    gill-attachment: attached=a,descending=d,free=f,notched=n

    gill-spacing: close=c,crowded=w,distant=d

    gill-size: broad=b,narrow=n

    gill-color: black=k,brown=n,buff=b,chocolate=h,gray=g, green=r,orange=o,pink=p,purple=u,red=e,white=w,yellow=y

    stalk-shape: enlarging=e,tapering=t

    stalk-root: bulbous=b,club=c,cup=u,equal=e,rhizomorphs=z,rooted=r,missing=?

    stalk-surface-above-ring: fibrous=f,scaly=y,silky=k,smooth=s

    stalk-surface-below-ring: fibrous=f,scaly=y,silky=k,smooth=s

    stalk-color-above-ring: brown=n,buff=b,cinnamon=c,gray=g,orange=o,pink=p,red=e,white=w,yellow=y

    stalk-color-below-ring: brown=n,buff=b,cinnamon=c,gray=g,orange=o,pink=p,red=e,white=w,yellow=y

    veil-type: partial=p,universal=u

    veil-color: brown=n,orange=o,white=w,yellow=y

    ring-number: none=n,one=o,two=t

    ring-type: cobwebby=c,evanescent=e,flaring=f,large=l,none=n,pendant=p,sheathing=s,zone=z

    spore-print-color: black=k,brown=n,buff=b,chocolate=h,green=r,orange=o,purple=u,white=w,yellow=y

    population: abundant=a,clustered=c,numerous=n,scattered=s,several=v,solitary=y

    habitat: grasses=g,leaves=l,meadows=m,paths=p,urban=u,waste=w,woods=d

}
\source{
UCI Machine Learning Repository
\link{https://archive.ics.uci.edu/ml/datasets/mushroom}
Originally taken from:
Mushroom records drawn from The Audubon Society Field Guide to North American Mushrooms (1981). G. H. Lincoff (Pres.), New York: Alfred A. Knopf
}
\references{
Schlimmer,J.S. (1987). Concept Acquisition Through Representational Adjustment (Technical Report 87-19). Doctoral disseration, Department of Information and Computer Science, University of California, Irvine.


Iba,W., Wogulis,J., & Langley,P. (1988). Trading off Simplicity and Coverage in Incremental Concept Learning. In Proceedings of the 5th International Conference on Machine Learning, 73-79. Ann Arbor, Michigan: Morgan Kaufmann.


Duch W, Adamczak R, Grabczewski K (1996) Extraction of logical rules from training data using backpropagation networks, in: Proc. of the The 1st Online Workshop on Soft Computing, 19-30.Aug.1996, pp. 25-30, [Web Link]


Duch W, Adamczak R, Grabczewski K, Ishikawa M, Ueda H, Extraction of crisp logical rules using constrained backpropagation networks - comparison of two new approaches, in: Proc. of the European Symposium on Artificial Neural Networks (ESANN'97), Bruge, Belgium 16-18.4.1997.

}
\examples{
data(mushrooms)
}
\keyword{datasets}
