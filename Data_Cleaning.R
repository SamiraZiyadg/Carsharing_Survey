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

#Save the File
write.csv(Data, "/Users/samiraziyadidegan/Burris-Carsharing/Data_Cleaned.csv", row.names = FALSE)

#Read the file
Data = as.data.frame(read_csv("/Users/samiraziyadidegan/Burris-Carsharing/Data_Cleaned_Cleaned.csv"))

#Imputation
## Walk to Campus (Minute)
### 5 minutes by Car and TAMU Bus == Walk 30 minutes  (5 rows)
### Other by Car and TAMU Bus == Walk 30+ minutes  (59 rows)
### Walk 15 minutes == walk 15 minutes  (1 only)
### Bike 15 minutes ==  30+ minutes (1 only)

#Travel_Time_Importance	Extremely important	
#Price_Importance	Extremely important	
#Convenience_Importance	Extremely important	
#Environmentally_Friendly_Importance	Moderately important

#Per_Hour_6	Strongly Agree	(2 rows)
#Per_Hour_8	Agree	(7 rows)
#Per_Hour_10	Disagree	(8 rows)
#Per_Hour_12	Strongly disagree (9 rows)

#Per_Day_40	Agree	(2 rows)
#Per_Day_50	Agree	(3 rows)
#Per_Day_60	Strongly disagree	(3 rows)
#Per_Day_70	Strongly disagree (3 rows)

#Per_Weekend_100	Strongly disagree	(5 rows)
#Per_Weekend_120	Strongly disagree	(5 rows)
#Per_Weekend_140	Strongly disagree (5 rows)

#Per_Semester_1750	Strongly disagree	(3 rows)
#Per_Semester_2000	Strongly disagree	(2 rows)
#Per_Semester_2250	Strongly disagree (2 rows)


Data$Driver_License = factor(Data$Driver_License)
Data$Vehicle_Ownership_Car = factor(Data$Vehicle_Ownership_Car)

Table = table(Data$Driver_License,Data$Vehicle_Ownership_Car)
CHI=chisq.test(as.matrix(Table))
CHI$p.value













