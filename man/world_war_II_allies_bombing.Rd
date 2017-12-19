\name{world_war_II_allies_bombing}
\alias{world_war_II_allies_bombing}
\docType{data}
\title{
World War II Aerial Bombing Data
}
\description{
This dataset consists of digitized paper mission reports from WWII. Each record includes the date, conflict, geographic location, and other data elements to form a live-action sequence of air warfare from 1939 to 1945. The records include U.S. and Royal Air Force data, in addition to some Australian, New Zealand and South African air force missions.

Warning: This data set has many missing entries!
}
\usage{data("world_war_II_allies_bombing")}
\format{
  A data frame with 178281 observations on the following 46 variables.
  \describe{
    \item{\samp{Mission ID}}{a numeric vector}
    \item{\samp{Mission Date}}{a character vector}
    \item{\samp{Theater of Operations}}{a factor with levels \code{CBI} \code{EAST AFRICA} \code{ETO} \code{MADAGASCAR} \code{MTO} \code{PTO}}
    \item{\code{Country}}{a factor with levels \code{AUSTRALIA} \code{GREAT BRITAIN} \code{NEW ZEALAND} \code{SOUTH AFRICA} \code{USA}}
    \item{\samp{Air Force}}{a character vector}
    \item{\samp{Unit ID}}{a character vector}
    \item{\samp{Aircraft Series}}{a factor with levels \code{0B17} \code{0B24} \code{100 SQ} \code{236/330 WINGS} \code{35 FG} \code{43 BG} \code{A20} \code{A24} \code{A26} \code{A36} \code{ALBA} \code{AUDA} \code{B17} \code{B24} \code{B25} \code{B26} \code{B29} \code{B32} \code{BALT} \code{BATT} \code{BEAU} \code{BEAUF} \code{BISLEY} \code{BLEN} \code{BOM} \code{BOST} \code{BOSTON} \code{CATALINA} \code{F06} \code{F4U} \code{GB17} \code{GB24} \code{HALI} \code{HAMP} \code{HUDSON} \code{HURR} \code{HVY} \code{JU86} \code{LB30} \code{LGT} \code{LIB} \code{LYSANDER} \code{MANC} \code{MARY} \code{MED} \code{MOHAWK} \code{OB17} \code{OB24} \code{P38} \code{P39} \code{P40} \code{P400} \code{P401} \code{P45} \code{P47} \code{P51} \code{P61} \code{P70} \code{PV-1 VENTURA} \code{SB17} \code{SB24} \code{SBD} \code{SBD DAUNTLESS} \code{STIR} \code{SUND} \code{SWORD} \code{TBF} \code{TBF AVENGER} \code{TOM} \code{VALE} \code{VENGEANCE (A31)} \code{WELL} \code{WHIT} \code{WIRRAWAY}}
    \item{\code{Callsign}}{a character vector}
    \item{\samp{Mission Type}}{a numeric vector}
    \item{\samp{Takeoff Base}}{a character vector}
    \item{\samp{Takeoff Location}}{a character vector}
    \item{\samp{Takeoff Latitude}}{a numeric vector}
    \item{\samp{Takeoff Longitude}}{a numeric vector}
    \item{\samp{Target ID}}{a numeric vector}
    \item{\samp{Target Country}}{a factor with many levels}
    \item{\samp{Target Type}}{a character vector}
    \item{\samp{Target Industry}}{a character vector}
    \item{\samp{Target Priority}}{a numeric vector}
    \item{\samp{Target Latitude}}{a numeric vector}
    \item{\samp{Target Longitude}}{a numeric vector}
    \item{\samp{Altitude (Hundreds of Feet)}}{a numeric vector}
    \item{\samp{Airborne Aircraft}}{a numeric vector}
    \item{\samp{Attacking Aircraft}}{a numeric vector}
    \item{\samp{Bombing Aircraft}}{a numeric vector}
    \item{\samp{Aircraft Returned}}{a character vector}
    \item{\samp{Aircraft Failed}}{a numeric vector}
    \item{\samp{Aircraft Damaged}}{a character vector}
    \item{\samp{Aircraft Lost}}{a character vector}
    \item{\samp{High Explosives}}{a numeric vector}
    \item{\samp{High Explosives Type}}{a character vector}
    \item{\samp{High Explosives Weight (Pounds)}}{a character vector}
    \item{\samp{High Explosives Weight (Tons)}}{a numeric vector}
    \item{\samp{Incendiary Devices}}{a numeric vector}
    \item{\samp{Incendiary Devices Type}}{a character vector}
    \item{\samp{Incendiary Devices Weight (Pounds)}}{a numeric vector}
    \item{\samp{Incendiary Devices Weight (Tons)}}{a numeric vector}
    \item{\samp{Fragmentation Devices}}{a numeric vector}
    \item{\samp{Fragmentation Devices Type}}{a character vector}
    \item{\samp{Fragmentation Devices Weight (Pounds)}}{a character vector}
    \item{\samp{Fragmentation Devices Weight (Tons)}}{a numeric vector}
    \item{\samp{Total Weight (Pounds)}}{a numeric vector}
    \item{\samp{Total Weight (Tons)}}{a numeric vector}
    \item{\samp{Time Over Target}}{a character vector}
    \item{\samp{Bomb Damage Assessment}}{a character vector}
    \item{\samp{Source ID}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Lt Col Jenns Robertson of the US Air Force developed the Theater History of Operations Reports (THOR) and posted them online after receiving Department of Defense approval.
}
\references{
\url{https://www.kaggle.com/usaf/world-war-ii}
}
\examples{
data(world_war_II_allies_bombing)
## maybe str(world_war_II_allies_bombing) ; plot(world_war_II_allies_bombing) ...
}
\keyword{datasets}
