

daily_fantasy_dataGrab<- function(currentWeek, currentYear=2018)  {

  #Add the current year as well
  currentYear=2018
  currentWeek=5

  #Create a moving average function
  MoveAvg <- function(x,n=3){if(length(x) >= n) {stats::filter(x,rep(1/n,n), sides=1)} else {NA_real_}}



  for (year in 2014:2017) {
    for (week in 1:17) {
      url.string=paste("http://rotoguru1.com/cgi-bin/fyday.pl?game=dk&scsv=1&week=", week, "&year=", year, sep="")
      betting.url<-paste("https://fantasydata.com/nfl-stats/point-spreads-and-odds?season=", year, "&seasontype=1&week=", week, sep="")
      webpage.betting<-read_html(betting.url)
      webpage<-read_html(url.string)
      xml.extraction=html_nodes(webpage, "pre")
      webpage.text=as.character(xml.extraction[1])
      webpage.text=unlist(strsplit(webpage.text, "<pre>"))[2]
      webpage.text<-unlist(strsplit(webpage.text, '<', fixed=TRUE))[1]
      ff=read.csv(text=webpage.text, sep=';')
      if (week==1 & year==2014) {
        ff.total=ff
      } else {
        ff.total=rbind(ff.total, ff)
      }
    }
  }



  for (week in 1:currentWeek) {
      url.string=paste("http://rotoguru1.com/cgi-bin/fyday.pl?game=dk&scsv=1&week=", week, "&year=", currentYear, sep="")
      webpage<-read_html(url.string)
      xml.extraction=html_nodes(webpage, "pre")
      webpage.text=as.character(xml.extraction[1])
      webpage.text=unlist(strsplit(webpage.text, "<pre>"))[2]
      webpage.text<-unlist(strsplit(webpage.text, '<', fixed=TRUE))[1]
      ff=read.csv(text=webpage.text, sep=';')
      ff.total=rbind(ff.total, ff)
  }

  #Add some columns with the moving averages of the Fantasy points
  draft_kings<-draft_kings %>% group_by(Name,Year) %>% mutate(rm2=maMy(DK.points,2))
  draft_kings<-draft_kings %>% group_by(Name,Year) %>% mutate(rm3=maMy(DK.points,3))
  draft_kings<-draft_kings %>% group_by(Name,Year) %>% mutate(rm5=maMy(DK.points,5))


  return(ff.total)
}
