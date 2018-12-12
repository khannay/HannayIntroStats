



letter.grade.convert<-function(num_grades) {

  ifelse(num_grades >=89.50, "A",
         ifelse(num_grades >=79.5, "B",
                ifelse(num_grades >=69.5, "C",
                       ifelse(num_grades>=59.5, "D", "F"))))


}
