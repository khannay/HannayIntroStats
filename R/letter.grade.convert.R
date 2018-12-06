



letter.grade.convert<-function(num_grades) {

  ifelse(num_grades >=90, "A",
         ifelse(num_grades >=80, "B",
                ifelse(num_grades >=70, "C",
                       ifelse(num_grades>=60, "D", "F"))))


}
