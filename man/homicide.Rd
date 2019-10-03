\name{homicide}
\alias{homicide}
\docType{data}
\title{
US Homicide Records
}
\description{
The Murder Accountability Project is the most complete database of homicides in the United States currently available. This dataset includes murders from the FBI's Supplementary Homicide Report from 1976 to the present and Freedom of Information Act data on more than 22,000 homicides that were not reported to the Justice Department. This dataset includes the age, race, sex, ethnicity of victims and perpetrators, in addition to the relationship between the victim and perpetrator and weapon used.

Covers 1980-2014
}
\usage{data("homicide")}
\format{
  A data frame with 638454 observations on the following 23 variables.
  \describe{
    \item{\code{Record_ID}}{a numeric vector}
    \item{\code{Agency_Code}}{a factor with many levels}
    \item{\code{Agency_Name}}{a factor with many levels }
    \item{\code{State}}{a factor with levels \code{Alabama} \code{Alaska} \code{Arizona} \code{Arkansas} \code{California} \code{Colorado} \code{Connecticut} \code{Delaware} \code{District of Columbia} \code{Florida} \code{Georgia} \code{Hawaii} \code{Idaho} \code{Illinois} \code{Indiana} \code{Iowa} \code{Kansas} \code{Kentucky} \code{Louisiana} \code{Maine} \code{Maryland} \code{Massachusetts} \code{Michigan} \code{Minnesota} \code{Mississippi} \code{Missouri} \code{Montana} \code{Nebraska} \code{Nevada} \code{New Hampshire} \code{New Jersey} \code{New Mexico} \code{New York} \code{North Carolina} \code{North Dakota} \code{Ohio} \code{Oklahoma} \code{Oregon} \code{Pennsylvania} \code{Rhodes Island} \code{South Carolina} \code{South Dakota} \code{Tennessee} \code{Texas} \code{Utah} \code{Vermont} \code{Virginia} \code{Washington} \code{West Virginia} \code{Wisconsin} \code{Wyoming}}
    \item{\code{Year}}{a numeric vector}
    \item{\code{Month}}{a factor with levels \code{April} \code{August} \code{December} \code{February} \code{January} \code{July} \code{June} \code{March} \code{May} \code{November} \code{October} \code{September}}
    \item{\code{Incident}}{a numeric vector}
    \item{\code{Crime_Type}}{a factor with levels \code{Manslaughter by Negligence} \code{Murder or Manslaughter}}
    \item{\code{Crime_Solved}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{Victim_Sex}}{a factor with levels \code{Female} \code{Male} \code{Unknown}}
    \item{\code{Victim_Age}}{a numeric vector}
    \item{\code{Victim_Race}}{a factor with levels \code{Asian/Pacific Islander} \code{Black} \code{Native American/Alaska Native} \code{Unknown} \code{White}}
    \item{\code{Victim_Ethnicity}}{a factor with levels \code{Hispanic} \code{Not Hispanic} \code{Unknown}}
    \item{\code{Perpetrator_Sex}}{a factor with levels \code{Female} \code{Male} \code{Unknown}}
    \item{\code{Perpetrator_Age}}{a numeric vector}
    \item{\code{Perpetrator_Race}}{a factor with levels \code{Asian/Pacific Islander} \code{Black} \code{Native American/Alaska Native} \code{Unknown} \code{White}}
    \item{\code{Perpetrator_Ethnicity}}{a factor with levels \code{Hispanic} \code{Not Hispanic} \code{Unknown}}
    \item{\code{Relationship}}{a factor with levels \code{Acquaintance} \code{Boyfriend} \code{Boyfriend/Girlfriend} \code{Brother} \code{Common-Law Husband} \code{Common-Law Wife} \code{Daughter} \code{Employee} \code{Employer} \code{Ex-Husband} \code{Ex-Wife} \code{Family} \code{Father} \code{Friend} \code{Girlfriend} \code{Husband} \code{In-Law} \code{Mother} \code{Neighbor} \code{Sister} \code{Son} \code{Stepdaughter} \code{Stepfather} \code{Stepmother} \code{Stepson} \code{Stranger} \code{Unknown} \code{Wife}}
    \item{\code{Weapon}}{a factor with levels \code{Blunt Object} \code{Drowning} \code{Drugs} \code{Explosives} \code{Fall} \code{Fire} \code{Firearm} \code{Gun} \code{Handgun} \code{Knife} \code{Poison} \code{Rifle} \code{Shotgun} \code{Strangulation} \code{Suffocation} \code{Unknown}}
    \item{\code{Victim_Count}}{a numeric vector}
    \item{\code{Perpetrator_Count}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
https://www.kaggle.com/jyzaguirre/us-homicide-reports
}
\references{
The data was compiled and made available by the Murder Accountability Project, founded by Thomas Hargrove.
}
\examples{
data(homicide)
}
\keyword{datasets}
