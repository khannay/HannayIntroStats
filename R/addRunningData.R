
newRunning <- readr::read_csv("~/Downloads/Activities(2).csv",
col_types = cols(`Activity Type` = col_skip(),
`Avg Vertical Oscillation` = col_skip(),
`Avg Vertical Ratio` = col_skip(),
`Best Lap Time` = col_skip(), `Bottom Time` = col_skip(),
Decompression = col_skip(), Favorite = col_skip(),
Flow = col_skip(), Grit = col_skip(),
`Max Run Cadence` = col_skip(), `Max Temp` = col_skip(),
`Min Temp` = col_skip(), `Number of Runs` = col_skip(),
`Surface Interval` = col_skip(),
Title = col_skip(), `Training Stress Score®` = col_skip()))

library(dplyr)
newRunning<-fixColumnNames(newRunning);
newRunning<-newRunning %>% select(Date, Distance, Calories, Time, Avg_HR, Max_HR, Avg_Run_Cadence, Avg_Pace, Best_Pace, Elev_Gain, Elev_Loss, Avg_Stride_Length)

newRunning$Best_Pace<-as.numeric(newRunning$Best_Pace)/3600.0
newRunning$Avg_Pace=as.numeric(newRunning$Avg_Pace)/3600.0

newRunning$Date<-as.Date(newRunning$Date)




