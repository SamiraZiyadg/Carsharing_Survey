library(readr)
library(MASS)
library(tidyverse)
library(data.table)
library(dplyr)

Data = as.data.frame(read_csv("/Users/samiraziyadidegan/Burris-Carsharing/TAMU_Carsharing_Project.csv"))
Data$Comments=NULL
Data$X51=NULL
Data$Others_Importance=NULL
Data$Others_Factor=NULL

Data=Data[Data$Progress == 100,]
Data=Data[Data$Response_Type == "IP Address",]
Data=Data[Data$Finished == "TRUE",]
Data=Data[Data$Student != "Not a student",]

length(Data[Data[,"Carsharing"] =="Yes" ,1])
length(Data[Data[,"Carsharing"] =="No" ,1])
length(Data[Data[,"Carsharing"] =="Maybe" ,1])

Data$Gender = factor(Data$Gender)
Data$Student = factor(Data$Student)
Data$Citizen = factor(Data$Citizen)
Data$Driver_License = factor(Data$Driver_License)
Data$Vehicle_Ownership_Car = factor(Data$Vehicle_Ownership_Car)
Data$Vehicle_Ownership_Bicycle = factor(Data$Vehicle_Ownership_Bicycle)
Data$Vehicle_Ownership_Motorbike = factor(Data$Vehicle_Ownership_Motorbike)
Data$Weekdays_to_Campus = factor(Data$Weekdays_to_Campus)
Data$Car_Weekdays_Alone_to_Campus = factor(Data$Car_Weekdays_Alone_to_Campus)
Data$Weekdays_Other_Trips = factor(Data$Weekdays_Other_Trips)
Data$Car_Weekdays_Alone_Other_Trips = factor(Data$Car_Weekdays_Alone_Other_Trips)
Data$Weekends_Mode = factor(Data$Weekends_Mode)
Data$Car_Weekends_Alone = factor(Data$Car_Weekends_Alone)
Data$Heard_Carsharing = factor(Data$Heard_Carsharing)
Data$Preferred_Carsharing_Period = factor(Data$Preferred_Carsharing_Period)

#Ordered
Data$Income=ordered(Data$Income, levels=c("Less_than_1500", '1500-2500', 'More_than_2500', "Prefer_not_to_say"))
Data$TTT_to_Campus_Regular_Mode=ordered(Data$TTT_to_Campus_Regular_Mode, levels=c('5 minutes','10 minutes','15 minutes','20 minutes','25 minutes','30 minutes','30+ minutes'))
Data$Walk_to_Campus_Minute=ordered(Data$Walk_to_Campus_Minute, levels=c('5 minutes','10 minutes','15 minutes','20 minutes','25 minutes','30 minutes','30+ minutes'))
Data$Travel_Time_Importance=ordered(Data$Travel_Time_Importance, levels=c('Not at all important', 'Slightly important', 'Moderately important','Very Important', 'Extremely important'))
Data$Price_Importance=ordered(Data$Price_Importance, levels=c('Not at all important', 'Slightly important', 'Moderately important','Very Important', 'Extremely important'))
Data$Convenience_Importance=ordered(Data$Convenience_Importance, levels=c('Not at all important', 'Slightly important', 'Moderately important','Very Important', 'Extremely important'))
Data$Environmentally_Friendly_Importance=ordered(Data$Environmentally_Friendly_Importance, levels=c('Not at all important', 'Slightly important', 'Moderately important','Very Important', 'Extremely important'))
Data$Two_Way_Carsharing_Cheapest_Rank=ordered(Data$Two_Way_Carsharing_Cheapest_Rank, levels=c("1","2","3"))
Data$One_Way_Medium_Rank=ordered(Data$One_Way_Medium_Rank, levels=c("1","2","3"))
Data$Free_Floating_High_price=ordered(Data$Free_Floating_High_price, levels=c("1","2","3"))
Data$Per_Hour_6=ordered(Data$Per_Hour_6, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Per_Hour_8=ordered(Data$Per_Hour_8, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Per_Hour_10=ordered(Data$Per_Hour_10, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Per_Day_40=ordered(Data$Per_Day_40, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Per_Day_50=ordered(Data$Per_Day_50, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Per_Day_60=ordered(Data$Per_Day_60, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Per_Day_70=ordered(Data$Per_Day_70, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Per_Weekend_90=ordered(Data$Per_Weekend_90, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Per_Weekend_100=ordered(Data$Per_Weekend_100, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Per_Weekend_120=ordered(Data$Per_Weekend_120, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Per_Weekend_140=ordered(Data$Per_Weekend_140, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Per_Semester_1500=ordered(Data$Per_Semester_1500, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Per_Semester_1750=ordered(Data$Per_Semester_1750, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Per_Semester_2000=ordered(Data$Per_Semester_2000, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Per_Semester_2250=ordered(Data$Per_Semester_2250, levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data$Walking_Time_Affect_Carsharing=ordered(Data$Walking_Time_Affect_Carsharing, levels=c("Extremely negative", 'Somewhat negative', 'Neither positive nor negative','Somewhat positive', 'Extremely positive'))
Data$Carsharing =ordered(Data$Carsharing, levels=c("No", "Maybe", "Yes"))

#Save the File
write.csv(Data, "/Users/samiraziyadidegan/Burris-Carsharing/Data_Cleaned.csv", row.names = FALSE)



