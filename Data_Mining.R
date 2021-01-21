library(readr)
library(MASS)
library(tidyverse)
library(data.table)
library(dplyr)

Data = as.data.frame(read_csv("/Users/samiraziyadidegan/Burris-Carsharing/Data_Cleaned.csv"))

#Descriptive Statistics
Data %>% 
  group_by(Gender) %>% 
  summarise(percent = 100*n()/nrow(Data))
Table=data.frame(table(Data$Gender,Data$Carsharing))
names(Table)[1]="Gender"
names(Table)[2]="Carsharing"
Table %>% 
  group_by(Carsharing) %>% 
  mutate_at(vars(Gender), funs("percent" = 100*Freq/sum(Freq)))

Data %>% 
  group_by(Student) %>% 
  summarise(percent = 100*n()/nrow(Data))
Table=data.frame(table(Data$Student,Data$Carsharing))
names(Table)[1]="Student"
names(Table)[2]="Carsharing"
Table %>% 
  group_by(Carsharing) %>% 
  mutate_at(vars(Student), funs("percent" = 100*Freq/sum(Freq)))

Data %>% 
  group_by(Citizen) %>% 
  summarise(percent = 100*n()/nrow(Data))
Table=data.frame(table(Data$Citizen,Data$Carsharing))
names(Table)[1]="Citizen"
names(Table)[2]="Carsharing"
Table %>% 
  group_by(Carsharing) %>% 
  mutate_at(vars(Citizen), funs("percent" = 100*Freq/sum(Freq)))

Data %>% 
  group_by(Driver_License) %>% 
  summarise(percent = 100*n()/nrow(Data))
Table=data.frame(table(Data$Driver_License,Data$Carsharing))
names(Table)[1]="Driver_License"
names(Table)[2]="Carsharing"
Table %>% 
  group_by(Carsharing) %>% 
  mutate_at(vars(Driver_License), funs("percent" = 100*Freq/sum(Freq)))

Data %>% 
  group_by(Income) %>% 
  summarise(percent = 100*n()/nrow(Data))
Table=data.frame(table(Data$Income,Data$Carsharing))
names(Table)[1]="Income"
names(Table)[2]="Carsharing"
Table %>% 
  group_by(Carsharing) %>% 
  mutate_at(vars(Income), funs("percent" = 100*Freq/sum(Freq)))

Data %>% 
  group_by(Heard_Carsharing) %>% 
  summarise(percent = 100*n()/nrow(Data))
Table=data.frame(table(Data$Heard_Carsharing,Data$Carsharing))
names(Table)[1]="Heard_Carsharing"
names(Table)[2]="Carsharing"
Table %>% 
  group_by(Carsharing) %>% 
  mutate_at(vars(Heard_Carsharing), funs("percent" = 100*Freq/sum(Freq)))

Data %>% 
  group_by(Vehicle_Ownership_Car, Vehicle_Ownership_Bicycle, Vehicle_Ownership_Motorbike) %>% 
  summarise(percent = 100*n()/nrow(Data))

Data %>% 
  group_by(Vehicle_Ownership_Car, Vehicle_Ownership_Bicycle, Vehicle_Ownership_Motorbike, Carsharing) %>% 
  summarise(Frequency = n())


#####

Data_Hourly=Data[Data$Preferred_Carsharing_Period == 'Per hour',]
Data_Daily=Data[Data$Preferred_Carsharing_Period == 'Per day',]
Data_Weekend=Data[Data$Preferred_Carsharing_Period == 'Per weekend',]
Data_Semester=Data[Data$Preferred_Carsharing_Period == 'Per semester',]

#Save the File
write.csv(Data_Hourly, "/Users/samiraziyadidegan/Burris-Carsharing/Data_Hourly.csv", row.names = FALSE)
write.csv(Data_Daily, "/Users/samiraziyadidegan/Burris-Carsharing/Data_Daily.csv", row.names = FALSE)
write.csv(Data_Weekend, "/Users/samiraziyadidegan/Burris-Carsharing/Data_Weekend.csv", row.names = FALSE)
write.csv(Data_Semester, "/Users/samiraziyadidegan/Burris-Carsharing/Data_Semester.csv", row.names = FALSE)


