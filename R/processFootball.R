nflplays <- read_csv("~/Downloads/NFL Play by Play 2009-2017 (v4).csv.zip", col_types = cols(Date = col_date(format = "%Y-%m-%d")))
nflplays=nflplays %>% select(Date:AwayTeam, Season)
nflplays %>% filter(PlayType %in% c('Run', 'Pass'))
nflplays2=nflplays %>% filter(PlayType %in% c('Run', 'Pass'))
nflplays=nflplays2 %>% select(Date:Touchdown, Safety, PlayType:Reception, Fumble,RecFumbTeam, Sack, Accepted.Penalty:Season )
nflplays=nflplays %>% filter(Accepted.Penalty==0)
nflplays$Accepted.Penalty=NULL
nflplays$PlayAttempted=NULL
rm(nflplays2)
nflplays$Passer_ID=NULL
nflplays$Rusher_ID=NULL
nflplays$RushAttempt=NULL
nflplays=nflplays %>% select(Date:Sack, PosTeamScore:Season)
nflplays$Receiver_ID=NULL
nflplays$TimeUnder=NULL
nflplays$FirstDown=NULL
nflplays$SideofField=NULL
nflplays$yrdln=NULL
nflplays$TimeSecs=NULL
nflplays$PassAttempt=NULL
nflplays$play.description=nflplays$desc
nflplays$desc=NULL
nflplays$posteam=as.factor(nflplays$posteam)
nflplays$DefensiveTeam=as.factor(nflplays$DefensiveTeam)
nflplays$PlayType=as.factor(nflplays$PlayType)
nflplays$Passer=as.factor(nflplays$Passer)
nflplays$PassOutcome=as.factor(nflplays$PassOutcome)
nflplays$PassLength=as.factor(nflplays$PassLength)
nflplays$QBHit=as.factor(nflplays$QBHit)
nflplays$PassLocation=as.factor(nflplays$PassLocation)
nflplays$InterceptionThrown=as.factor(nflplays$InterceptionThrown)
nflplays$Rusher=as.factor(nflplays$Rusher)
nflplays$RunLocation=as.factor(nflplays$RunLocation)
nflplays$RunGap=as.factor(nflplays$RunGap)
nflplays$Receiver=as.factor(nflplays$Receiver)
nflplays$Reception=as.factor(nflplays$Reception)
nflplays$HomeTeam=as.factor(nflplays$HomeTeam)
nflplays$AwayTeam=as.factor(nflplays$AwayTeam)
nflplays$Season=as.factor(nflplays$Season)





