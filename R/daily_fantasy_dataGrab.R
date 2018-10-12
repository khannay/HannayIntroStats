

daily_fantasy_dataGrab<- function(currentWeek, currentYear=2018)  {

  #Add the current year as well
  currentYear=2018
  currentWeek=5
  
  
  for (year in 2014:2017) {
    for (week in 1:17) {
      url.string=paste("http://rotoguru1.com/cgi-bin/fyday.pl?game=dk&scsv=1&week=", week, "&year=", year, sep="")
      webpage<-read_html(url.string)
      xml.extraction=html_nodes(webpage, "pre")
      webpage.text=as.character(xml.extraction[1])
      webpage.text=unlist(strsplit(webpage.text, "<pre>"))[2]
      webpage.text<-unlist(strsplit(webpage.text, '<', fixed=TRUE))[1]
      ff=read.csv(text=webpage.text, sep=';')
      if (week!=1 & year!=2014) {
        ff.total=rbind(ff.total, ff)
      } else {
        ff.total=ff
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
  return(ff.total)
}
