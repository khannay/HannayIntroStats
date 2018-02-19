\name{GlobalFirePower}
\alias{GlobalFirePower}
\docType{data}
\title{
Global Military Fire Power Scores
}
\description{
This data set provides an overview of the military strength of 133 countries around the world.
}
\usage{data("GlobalFirePower")}
\format{
  A data frame with 133 observations on the following 47 variables.
  \describe{
    \item{\code{Country}}{a character vector giving the country name}
    \item{\code{Country.Code}}{a character vector giving the country code}
    \item{\code{Mil.Rank}}{a numeric vector the world rank of the military}
    \item{\code{Total.Population}}{a numeric vector giving the population of the country}
    \item{\code{Manpower.Available}}{a numeric vector giving the total manpower available in the country}
    \item{\code{Fit.for.Service}}{a numeric vector number of people fit for military service}
    \item{\code{Reaching.Military.Age}}{a numeric vector giving the number of people who will soon reach mil age}
    \item{\code{Total.Military.Personnel}}{a numeric vector current total forces. }
    \item{\code{Active.Personnel}}{a numeric vector number of active duty forces}
    \item{\code{Reserve.Personnel}}{a numeric vector}
    \item{\code{Total.Aircraft.Strength}}{a numeric vector}
    \item{\code{Fighter.Aircraft}}{a numeric vector}
    \item{\code{Attack.Aircraft}}{a numeric vector}
    \item{\code{Transport.Aircraft}}{a numeric vector}
    \item{\code{Trainer.Aircraft}}{a numeric vector}
    \item{\code{Total.Helicopter.Strength}}{a numeric vector}
    \item{\code{Attack.Helicopters}}{a numeric vector}
    \item{\code{Combat.Tanks}}{a numeric vector}
    \item{\code{Armored.Fighting.Vehicles}}{a numeric vector}
    \item{\code{Self.Propelled.Artillery}}{a numeric vector}
    \item{\code{Towed.Artillery}}{a numeric vector}
    \item{\code{Rocket.Projectors}}{a numeric vector}
    \item{\code{Total.Naval.Assets}}{a numeric vector}
    \item{\code{Aircraft.Carriers}}{a numeric vector}
    \item{\code{Frigates}}{a numeric vector}
    \item{\code{Destroyers}}{a numeric vector}
    \item{\code{Corvettes}}{a numeric vector}
    \item{\code{Submarines}}{a numeric vector}
    \item{\code{Patrol.Craft}}{a numeric vector}
    \item{\code{Mine.Warfare.Vessels}}{a numeric vector}
    \item{\code{Production}}{a numeric vector}
    \item{\code{Consumption}}{a numeric vector}
    \item{\code{Proven.Reserves}}{a numeric vector}
    \item{\code{Labor.Force}}{a numeric vector}
    \item{\code{Merchant.Marine.Strength}}{a numeric vector}
    \item{\code{Major.Ports.and.Terminals}}{a numeric vector}
    \item{\code{Roadway.Coverage.km}}{a numeric vector}
    \item{\code{Railway.Coverage.km}}{a numeric vector}
    \item{\code{Serivecable.Airports}}{a numeric vector}
    \item{\code{Defense.Budget}}{a numeric vector}
    \item{\code{External.Debt}}{a numeric vector}
    \item{\samp{Foreign Exchange.Gold}}{a numeric vector}
    \item{\code{Purchasing.Power.Parity}}{a numeric vector}
    \item{\code{Square.Land.Area.km}}{a numeric vector}
    \item{\code{Coastline.km}}{a numeric vector}
    \item{\code{Shared.Borders.km}}{a numeric vector}
    \item{\code{Waterways.km}}{a numeric vector}
  }
}
\details{

    Manpower
        Total Populations
        Available Manpower
        Manpower Fit-for-Service
        Manpower Reaching Military Age Annually
        Active Military Manpower
        Active Reserve Military Manpower
    Air Power
        Total Aircraft Strength
        Fighters & Interceptors
        Attack Aircraft
        Transports
        Trainers
        Total Helicopters
        Attack Helicopters
        Serviceable Airports
    Army Strengths
        Combat Tanks
        Armored Fighting Vehicles
        Self-Propelled Artillery
        Towed Artillery
        Rocket Projectors
    Naval Power
        Total Naval Strength
        Aircraft Carriers
        Frigates
        Destroyers
        Corvettes
        Submarines
        Patrol Craft
        Mine Warfare
    Financial Resources
        Annual Defense Budgets
        External Debt
        Reserves of Foreign Exchange and Gold
        Purchasing Power Parity
    Logistical Resources
        Labor Force Strength
        Merchant Marine Strength
        Major Ports & Terminals
        Roadway Coverage
        Railway Coverage
    Natural Resources
        Oil Production
        Oil Consumption
        Proven Oil Reserves
    Geography
        Square Land Areas
        Coastline
        Shared Borders
        Waterway Coverage
 The finalized Global Firepower ranking relies on over 50 factors to determine a given nation's PowerIndex ('PwrIndx') score. Our formula allows smaller, though more technologically-advanced, nations to compete with larger, lesser-developed ones. Modifiers (in the form of bonuses and penalties) are added to further refine the list. Some items to observe in regards to the finalized ranking:

    Ranking does not simply rely on the total number of weapons available to any one country but rather focuses on weapon diversity within the number totals to provide a better balance of firepower available (i.e. fielding 100 minesweepers does not equal the strategic and tactical value of fielding 10 aircraft carriers).
    Nuclear stockpiles are NOT taken into account but recognized / suspected nuclear powers receive a bonus.
    Geographical factors, logistical flexibility, natural resources and local industry influence the final ranking.
    Available manpower is a key consideration; nations with large populations tend to rank higher.
    Land-locked nations are NOT penalized for lack of a navy; naval powers ARE penalized for lack of diversity in available assets.
    NATO allies receive a slight bonus due to the theoretical sharing of resources.
    Current political / military leadership is NOT taken into account.

}
\source{
The CSV files were parsed from http://www.globalfirepower.com/countries-listing.asp
}
\references{
\url{https://www.kaggle.com/blitzr/gfp2017}
}
\examples{
data(GlobalFirePower)
}
\keyword{datasets}
