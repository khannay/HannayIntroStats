

addClassesEnrollment <- function(CSVFile, semesterCode='Fall', yearData=2019) {
  spring2019_enrollment <- readr::read_csv(CSVFile)
  spring2019_enrollment$Faculty=as.factor(spring2019_enrollment$Faculty)
  spring2019_enrollment$Status=as.factor(spring2019_enrollment$Status)

  spring2019_enrollment<- spring2019_enrollment %>% filter(Name!="Name")

  spring2019_enrollment$Credits <- as.numeric(spring2019_enrollment$Credits)
  num.classes=dim(spring2019_enrollment)[1]

  sp2<-tidyr::separate(spring2019_enrollment, `Course code`, sep=' ', into=c('Department', 'Course.Num', 'Section.Num'), remove=FALSE)
  sp2=tidyr::separate(sp2, `Seats Open`, sep='/', into=c('Open.Seats', 'Enroll.Cap'), remove=TRUE, convert=TRUE)
  sp2$Students=sp2$Enroll.Cap-sp2$Open.Seats
  sp2$Semester<-rep(semesterCode, num.classes)
  sp2$Year<-rep(yearData, num.classes)
  sp2$Semester<-as.factor(sp2$Semester)
  sp2$Delivery=rep('FTF', num.classes)
  sp2$Small.Class.Less.Than.8=ifelse(sp2$Students<8, 1, 0)
  sp2$Is.Lab <- grepl("LAB", sp2$Name)
  sp2$Is.Intern <- grepl("Intern", sp2$Name)
  sp2$Faculty.Load=ifelse(sp2$Is.Lab, sp2$Credits*1.34, sp2$Credits)
  sp2$True.Load=ifelse(sp2$Students<6, sp2$Faculty.Load*sp2$Students/6.0, sp2$Faculty.Load);
  return(sp2)
}

