library(readr)
library(MASS)
library(tidyverse)
library(data.table)
library(dplyr)

Data = as.data.frame(read_csv("/Users/samiraziyadidegan/Burris-Carsharing/Data_Cleaned_Cleaned.csv"))

Data_Hourly=Data[Data$Preferred_Carsharing_Period == 'Per hour',]
Data_Daily=Data[Data$Preferred_Carsharing_Period == 'Per day',]
Data_Weekend=Data[Data$Preferred_Carsharing_Period == 'Per weekend',]
Data_Semester=Data[Data$Preferred_Carsharing_Period == 'Per semester',]

#Save the File
write.csv(Data_Hourly, "/Users/samiraziyadidegan/Burris-Carsharing/Data_Hourly.csv", row.names = FALSE)
write.csv(Data_Daily, "/Users/samiraziyadidegan/Burris-Carsharing/Data_Daily.csv", row.names = FALSE)
write.csv(Data_Weekend, "/Users/samiraziyadidegan/Burris-Carsharing/Data_Weekend.csv", row.names = FALSE)
write.csv(Data_Semester, "/Users/samiraziyadidegan/Burris-Carsharing/Data_Semester.csv", row.names = FALSE)

