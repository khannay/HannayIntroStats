\name{gestation_data}
\alias{gestation_data}
\docType{data}
\title{
Human Gestation Period Data
}
\description{
Do babies come on their due dates?

The rule of thumb for human pregancy length (gestation) is 40 weeks (280 days) although this can vary quite a bit. This data set has survey data from over 13000 expecting mothers on their gestation period.
}
\usage{data("gestation_data")}
\format{
  A data frame with 13550 observations on the following 19 variables.
  \describe{
    \item{\code{id}}{a numeric vector}
    \item{\code{surveydate}}{a numeric vector}
    \item{\code{IP_ID}}{a numeric vector}
    \item{\code{firstname_ID}}{a numeric vector}
    \item{\code{country}}{a factor with levels \code{Africa} \code{Argentina} \code{australia} \code{Australia} \code{AUSTRALIA} \code{Austria} \code{Bahamas} \code{Belgium} \code{Bishop} \code{Bosnia} \code{Brasil} \code{Brazil} \code{Brunei} \code{bulgaria} \code{Bulgaria} \code{canada} \code{Canada} \code{CAnada} \code{CANADA} \code{Chile} \code{China} \code{Colombia} \code{Costa Rica} \code{Croatia} \code{curacao} \code{Czech Republic} \code{Denmark} \code{Dubai} \code{Egypt} \code{Eire} \code{Estonia} \code{europe} \code{Finland} \code{france} \code{France} \code{Germany} \code{GERMANY} \code{greece} \code{Greece} \code{Guatemala} \code{Guyana} \code{holland} \code{Holland} \code{Hong Kong} \code{Hungary} \code{Iceland} \code{india} \code{India} \code{Indonesia} \code{ireland} \code{Ireland} \code{Israel} \code{italy} \code{Italy} \code{Jamaica} \code{Japan} \code{Kazakhstan} \code{kenya} \code{Kenya} \code{Korea} \code{kuwait} \code{Kuwait} \code{Latvia} \code{Lithuania} \code{Malaysia} \code{Mexico} \code{netherlands} \code{Netherlands} \code{new zealand} \code{New zealand} \code{New Zealand} \code{Nigeria} \code{northern ireland} \code{Northern Ireland} \code{Norway} \code{pakistan} \code{Pakistan} \code{Peru} \code{Philippines} \code{Poland} \code{portugal} \code{Portugal} \code{qatar} \code{Romania} \code{russia} \code{Russia} \code{Saudi Arabia} \code{Scotland} \code{Serbia} \code{Singapore} \code{Slovakia} \code{slovenia} \code{Slovenia} \code{south africa} \code{south Africa} \code{South Africa} \code{South Korea} \code{spain} \code{Spain} \code{Sverige} \code{Sweden} \code{Switzerland} \code{Trinidad} \code{Tunisia} \code{Uae} \code{UAE} \code{Ukraine} \code{United Kingdom} \code{United States} \code{URUGUAY} \code{Wills} \code{Zambia} \code{zimbabwe}}
    \item{\code{motherbirthyear}}{a numeric vector}
    \item{\code{multiples}}{a numeric vector}
    \item{\code{pounds}}{a numeric vector}
    \item{\code{ounces}}{a numeric vector}
    \item{\code{grams}}{a numeric vector}
    \item{\code{previousbirths}}{a numeric vector}
    \item{\code{bornonday}}{a numeric vector}
    \item{\code{duedate}}{a numeric vector}
    \item{\code{birthdate}}{a numeric vector}
    \item{\code{duedatebase}}{a factor with levels \code{LMP} \code{NULL} \code{Other} \code{ovulation} \code{ultrasound}}
    \item{\code{induced}}{a numeric vector}
    \item{\code{cesarean}}{a numeric vector}
    \item{\code{sex}}{a factor with levels \code{boy} \code{girl} \code{NULL}}
    \item{\code{error_bd_dd}}{a numeric vector}
  }
}
\details{
id	Unique ID for this survey entry
surveydate	Unix timestamp
IP ID	Each IP address is assigned an ID
firstname ID	Each name is assigned an ID. This is to help with duplicates.
country	Country
motherbirthyear	Mother's birth year
motherbirthdate	Mother's birth date as a Unix timestamp. For confidentiality reasons I don't store exact birthdate, I pretend everyone was born on the 15th of the month.
multiples	How many babies were being born. 2 = twins, 3 triplets
pounds	Baby weight in pounds
ounces	Additional ounces
grams	Baby weight in grams
previousbirths	How many births the mother had before this one. 0 = first baby.
previousbornonday	Based on IP address, names and dates, this is the born on day for the mother's previous birth.
duedate	Due date
lmpdate	Last menstrual period date
birthdate	Date baby was born
bornonday	Gestation time in days - difference between the date the baby was born and LMP date. If LMP date isn't known we assumed it was 280 days before the due date.
duedatebase	Whether due date was determined by ovulation, LMP, or ultrasound
induced	Induced = 1, spontaneous = 0
cesarean	Cesarean = 1
sex	Boy or Girl

}
\source{
https://spacefem.com/pregnant/charts/duedate40.php
https://spacefem.com/pregnant/charts/duedate0.php
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(gestation_data)

}
\keyword{datasets}
