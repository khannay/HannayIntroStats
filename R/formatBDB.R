#' Format Basketball Reference.com spreadsheets
#'
#' This will format the Basketball References CSV downloads for statistical analysis and return a new data frame.
#'
#' @param mydf this is the data frame to format
#' @return a data frame formatted
#' @examples
#' Kawhi_Leonard_2016=formatBDB(Leonard_2016_2017)
#' @export

formatBDB<- function(mydf) {
    mydf$Rk<-NULL
    mydf$Game.Result=mydf$X8
    mydf$Plus.Minus=mydf$`+/-`
    mydf$X8<-NULL
    mydf$`+/-`<-NULL
    mydf$G<-NULL
    mydf$X6<-as.character(mydf$X6)
    mydf$GS<-as.factor(mydf$GS)

    mydf$X6[is.na(mydf$X6)]<-"A"
    mydf$X6[mydf$X6=="@"]<-"H"
    mydf$Location=as.factor(mydf$X6)
    mydf$X6<-NULL
    mydf[,7:25]=sapply(mydf[,7:25], as.numeric)
    mydf$Plus.Minus=as.integer(mydf$Plus.Minus)
    mydf$Opp=as.factor(mydf$Opp)
    minPerGame=mydf$MP
    LMP=sapply(strsplit(minPerGame,":"),function(x) { x <- as.numeric(x); x[1]+x[2]/60;});

    mydf$MP=LMP
    GR=mydf$Game.Result
    mydf$Game.Result=as.factor(sapply(strsplit(GR, " "), function(x) { x[1];}))
    Game.Margin=sapply(strsplit(GR, " "), function(x) { x[2]})
    mydf$Game.Margin=as.integer(substring(Game.Margin, 2, 3))
    return(data.frame(mydf))

}
