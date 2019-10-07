\name{fifa}
\alias{fifa}
\docType{data}
\title{
FIFA 19 Complete data
}
\description{
Detailed attributes for every player registered in the latest edition of FIFA 19 database.
}
\usage{data("fifa")}
\format{
  A data frame with 18207 observations on the following 85 variables.
  \describe{
    \item{\code{ID}}{a numeric vector}
    \item{\code{Name}}{a character vector}
    \item{\code{Age}}{a numeric vector}
    \item{\code{Nationality}}{a factor with levels \code{Afghanistan} \code{Albania} \code{Algeria} \code{Andorra} \code{Angola} \code{Antigua  Barbuda} \code{Argentina} \code{Armenia} \code{Australia} \code{Austria} \code{Azerbaijan} \code{Barbados} \code{Belarus} \code{Belgium} \code{Belize} \code{Benin} \code{Bermuda} \code{Bolivia} \code{Bosnia Herzegovina} \code{Botswana} \code{Brazil} \code{Bulgaria} \code{Burkina Faso} \code{Burundi} \code{Cameroon} \code{Canada} \code{Cape Verde} \code{Central African Rep.} \code{Chad} \code{Chile} \code{China PR} \code{Colombia} \code{Comoros} \code{Congo} \code{Costa Rica} \code{Croatia} \code{Cuba} \code{Curacao} \code{Cyprus} \code{Czech Republic} \code{Denmark} \code{Dominican Republic} \code{DR Congo} \code{Ecuador} \code{Egypt} \code{El Salvador} \code{England} \code{Equatorial Guinea} \code{Eritrea} \code{Estonia} \code{Ethiopia} \code{Faroe Islands} \code{Fiji} \code{Finland} \code{France} \code{FYR Macedonia} \code{Gabon} \code{Gambia} \code{Georgia} \code{Germany} \code{Ghana} \code{Greece} \code{Grenada} \code{Guam} \code{Guatemala} \code{Guinea} \code{Guinea Bissau} \code{Guyana} \code{Haiti} \code{Honduras} \code{Hong Kong} \code{Hungary} \code{Iceland} \code{India} \code{Indonesia} \code{Iran} \code{Iraq} \code{Israel} \code{Italy} \code{Ivory Coast} \code{Jamaica} \code{Japan} \code{Jordan} \code{Kazakhstan} \code{Kenya} \code{Korea DPR} \code{Korea Republic} \code{Kosovo} \code{Kuwait} \code{Latvia} \code{Lebanon} \code{Liberia} \code{Libya} \code{Liechtenstein} \code{Lithuania} \code{Luxembourg} \code{Madagascar} \code{Mali} \code{Malta} \code{Mauritania} \code{Mauritius} \code{Mexico} \code{Moldova} \code{Montenegro} \code{Montserrat} \code{Morocco} \code{Mozambique} \code{Namibia} \code{Netherlands} \code{New Caledonia} \code{New Zealand} \code{Nicaragua} \code{Niger} \code{Nigeria} \code{Northern Ireland} \code{Norway} \code{Oman} \code{Palestine} \code{Panama} \code{Paraguay} \code{Peru} \code{Philippines} \code{Poland} \code{Portugal} \code{Puerto Rico} \code{Qatar} \code{Republic of Ireland} \code{Romania} \code{Russia} \code{Rwanda} \code{São Tomé & Príncipe} \code{Saudi Arabia} \code{Scotland} \code{Senegal} \code{Serbia} \code{Sierra Leone} \code{Slovakia} \code{Slovenia} \code{South Africa} \code{South Sudan} \code{Spain} \code{St Kitts Nevis} \code{St Lucia} \code{Sudan} \code{Suriname} \code{Sweden} \code{Switzerland} \code{Syria} \code{Tanzania} \code{Thailand} \code{Togo} \code{Trinidad & Tobago} \code{Tunisia} \code{Turkey} \code{Uganda} \code{Ukraine} \code{United Arab Emirates} \code{United States} \code{Uruguay} \code{Uzbekistan} \code{Venezuela} \code{Wales} \code{Zambia} \code{Zimbabwe}}
    \item{\code{Overall}}{a numeric vector}
    \item{\code{Potential}}{a numeric vector}
    \item{\code{Club}}{a factor with many levels}
    \item{\code{Value}}{a character vector}
    \item{\code{Wage}}{a character vector}
    \item{\code{Special}}{a numeric vector}
    \item{\code{Preferred_Foot}}{a factor with levels \code{Left} \code{Right}}
    \item{\code{International_Reputation}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5}}
    \item{\code{Weak_Foot}}{a numeric vector}
    \item{\code{Skill_Moves}}{a numeric vector}
    \item{\code{Work_Rate}}{a factor with levels \code{High/ High} \code{High/ Low} \code{High/ Medium} \code{Low/ High} \code{Low/ Low} \code{Low/ Medium} \code{Medium/ High} \code{Medium/ Low} \code{Medium/ Medium}}
    \item{\code{Body_Type}}{a factor with levels \code{Akinfenwa} \code{C. Ronaldo} \code{Courtois} \code{Lean} \code{Messi} \code{Neymar} \code{Normal} \code{PLAYER_BODY_TYPE_25} \code{Shaqiri} \code{Stocky}}
    \item{\code{Real_Face}}{a character vector}
    \item{\code{Position}}{a factor with levels \code{CAM} \code{CB} \code{CDM} \code{CF} \code{CM} \code{GK} \code{LAM} \code{LB} \code{LCB} \code{LCM} \code{LDM} \code{LF} \code{LM} \code{LS} \code{LW} \code{LWB} \code{RAM} \code{RB} \code{RCB} \code{RCM} \code{RDM} \code{RF} \code{RM} \code{RS} \code{RW} \code{RWB} \code{ST}}
    \item{\code{Jersey_Number}}{a numeric vector}
    \item{\code{Joined}}{a character vector}
    \item{\code{Loaned_From}}{a character vector}
    \item{\code{Contract_Valid_Until}}{a character vector}
    \item{\code{Height}}{a character vector}
    \item{\code{Weight}}{a character vector}
    \item{\code{LS}}{a character vector}
    \item{\code{ST}}{a character vector}
    \item{\code{RS}}{a character vector}
    \item{\code{LW}}{a character vector}
    \item{\code{LF}}{a character vector}
    \item{\code{CF}}{a character vector}
    \item{\code{RF}}{a character vector}
    \item{\code{RW}}{a character vector}
    \item{\code{LAM}}{a character vector}
    \item{\code{CAM}}{a character vector}
    \item{\code{RAM}}{a character vector}
    \item{\code{LM}}{a character vector}
    \item{\code{LCM}}{a character vector}
    \item{\code{CM}}{a character vector}
    \item{\code{RCM}}{a character vector}
    \item{\code{RM}}{a character vector}
    \item{\code{LWB}}{a character vector}
    \item{\code{LDM}}{a character vector}
    \item{\code{CDM}}{a character vector}
    \item{\code{RDM}}{a character vector}
    \item{\code{RWB}}{a character vector}
    \item{\code{LB}}{a character vector}
    \item{\code{LCB}}{a character vector}
    \item{\code{CB}}{a character vector}
    \item{\code{RCB}}{a character vector}
    \item{\code{RB}}{a character vector}
    \item{\code{Crossing}}{a numeric vector}
    \item{\code{Finishing}}{a numeric vector}
    \item{\code{HeadingAccuracy}}{a numeric vector}
    \item{\code{ShortPassing}}{a numeric vector}
    \item{\code{Volleys}}{a numeric vector}
    \item{\code{Dribbling}}{a numeric vector}
    \item{\code{Curve}}{a numeric vector}
    \item{\code{FKAccuracy}}{a numeric vector}
    \item{\code{LongPassing}}{a numeric vector}
    \item{\code{BallControl}}{a numeric vector}
    \item{\code{Acceleration}}{a numeric vector}
    \item{\code{SprintSpeed}}{a numeric vector}
    \item{\code{Agility}}{a numeric vector}
    \item{\code{Reactions}}{a numeric vector}
    \item{\code{Balance}}{a numeric vector}
    \item{\code{ShotPower}}{a numeric vector}
    \item{\code{Jumping}}{a numeric vector}
    \item{\code{Stamina}}{a numeric vector}
    \item{\code{Strength}}{a numeric vector}
    \item{\code{LongShots}}{a numeric vector}
    \item{\code{Aggression}}{a numeric vector}
    \item{\code{Interceptions}}{a numeric vector}
    \item{\code{Positioning}}{a numeric vector}
    \item{\code{Vision}}{a numeric vector}
    \item{\code{Penalties}}{a numeric vector}
    \item{\code{Composure}}{a numeric vector}
    \item{\code{Marking}}{a numeric vector}
    \item{\code{StandingTackle}}{a numeric vector}
    \item{\code{SlidingTackle}}{a numeric vector}
    \item{\code{GKDiving}}{a numeric vector}
    \item{\code{GKHandling}}{a numeric vector}
    \item{\code{GKKicking}}{a numeric vector}
    \item{\code{GKPositioning}}{a numeric vector}
    \item{\code{GKReflexes}}{a numeric vector}
    \item{\code{Release_Clause}}{a character vector}
  }
}
\details{

Name: name
Age: age

Nationality: nationality

Overall: overall rating
Potential: potential rating
Club: current club

Value: current market value
Wage

current wage: Special

special: Preferred Foot

left/right
International Reputation

rating on scale of 5
Weak Foot

rating on scale of 5
Skill Moves

rating on scale of 5
Work Rate

attack work rate/defence work rate
Body Type

body type of player
Real Face

true or false
Position

position on the pitch
Jersey Number

jersey number
Joined

joined date
Loaned From

club name if applicable
Contract Valid Until

contract end date
Height

height of the player
Weight

weight of the player
LS

rating on scale of 100
ST

rating on scale of 100
RS

rating on scale of 100
LW

rating on scale of 100
LF

rating on scale of 100
CF

rating on scale of 100
RF

rating on scale of 100
RW

rating on scale of 100
LAM

rating on scale of 100
CAM

rating on scale of 100
RAM

rating on scale of 100
LM

rating on scale of 100
LCM

rating on scale of 100
CM

rating on scale of 100
RCM

rating on scale of 100
RM

rating on scale of 100
LWB

rating on scale of 100
LDM

rating on scale of 100
CDM

rating on scale of 100
RDM

rating on scale of 100
RWB

rating on scale of 100
LB

rating on scale of 100
LCB

rating on scale of 100
CB

rating on scale of 100
RCB

rating on scale of 100
RB

rating on scale of 100
Crossing

rating on scale of 100
Finishing

rating on scale of 100
HeadingAccuracy

rating on scale of 100
ShortPassing

rating on scale of 100
Volleys

rating on scale of 100
Dribbling

rating on scale of 100
Curve

rating on scale of 100
FKAccuracy

rating on scale of 100
LongPassing

rating on scale of 100
BallControl

rating on scale of 100
Acceleration

rating on scale of 100
SprintSpeed

rating on scale of 100
Agility

rating on scale of 100
Reactions

rating on scale of 100
Balance

rating on scale of 100
ShotPower

rating on scale of 100
Jumping

rating on scale of 100
Stamina

rating on scale of 100
Strength

rating on scale of 100
LongShots

rating on scale of 100
Aggression

rating on scale of 100
Interceptions

rating on scale of 100
Positioning

rating on scale of 100
Vision

rating on scale of 100
Penalties

rating on scale of 100
Composure

rating on scale of 100
Marking

rating on scale of 100
StandingTackle

rating on scale of 100
SlidingTackle

rating on scale of 100
GKDiving

rating on scale of 100
GKHandling

rating on scale of 100
GKKicking

rating on scale of 100
GKPositioning

rating on scale of 100
GKReflexes

rating on scale of 100
Release Clause

release clause value
}
\source{
https://www.kaggle.com/karangadiya/fifa19
}
\references{
Data scraped from \url{https://sofifa.com}
}
\examples{
data(fifa)
}
\keyword{datasets}
