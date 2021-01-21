library(readr)
library(MASS)
library(tidyverse)
library(data.table)
library(dplyr)

#Read the files
Data_Daily = as.data.frame(read_csv("/Users/samiraziyadidegan/Burris-Carsharing/Daily/Data_Daily.csv"))

Data_Daily$Gender = factor(Data_Daily$Gender)
Data_Daily$Student = factor(Data_Daily$Student)
Data_Daily$Citizen = factor(Data_Daily$Citizen)
Data_Daily$Driver_License = factor(Data_Daily$Driver_License)
Data_Daily$Vehicle_Ownership_Car = factor(Data_Daily$Vehicle_Ownership_Car)
Data_Daily$Vehicle_Ownership_Bicycle = factor(Data_Daily$Vehicle_Ownership_Bicycle)
Data_Daily$Vehicle_Ownership_Motorbike = factor(Data_Daily$Vehicle_Ownership_Motorbike)
Data_Daily$Weekdays_to_Campus = factor(Data_Daily$Weekdays_to_Campus)
Data_Daily$Weekdays_Other_Trips = factor(Data_Daily$Weekdays_Other_Trips)
Data_Daily$Weekends_Mode = factor(Data_Daily$Weekends_Mode)
Data_Daily$Heard_Carsharing = factor(Data_Daily$Heard_Carsharing)

#Ordered
Data_Daily$Income=factor(Data_Daily$Income, ordered=TRUE, levels=c("Less_than_1500", '1500-2500', 'More_than_2500', "Prefer_not_to_say"))
Data_Daily$TTT_to_Campus_Regular_Mode=factor(Data_Daily$TTT_to_Campus_Regular_Mode, ordered=TRUE,levels=c('5 minutes','10 minutes','15 minutes','20 minutes','25 minutes','30 minutes','30+ minutes'))
Data_Daily$Walk_to_Campus_Minute=factor(Data_Daily$Walk_to_Campus_Minute,ordered=TRUE, levels=c('5 minutes','10 minutes','15 minutes','20 minutes','25 minutes','30 minutes','30+ minutes'))
Data_Daily$Travel_Time_Importance=factor(Data_Daily$Travel_Time_Importance, ordered=TRUE,levels=c('Not at all important', 'Slightly important', 'Moderately important','Very Important', 'Extremely important'))
Data_Daily$Price_Importance=factor(Data_Daily$Price_Importance, ordered=TRUE,levels=c('Not at all important', 'Slightly important', 'Moderately important','Very Important', 'Extremely important'))
Data_Daily$Convenience_Importance=factor(Data_Daily$Convenience_Importance,ordered=TRUE, levels=c('Not at all important', 'Slightly important', 'Moderately important','Very Important', 'Extremely important'))
Data_Daily$Environmentally_Friendly_Importance=factor(Data_Daily$Environmentally_Friendly_Importance, ordered=TRUE,levels=c('Not at all important', 'Slightly important', 'Moderately important','Very Important', 'Extremely important'))
Data_Daily$Two_Way_Carsharing_Cheapest_Rank=factor(Data_Daily$Two_Way_Carsharing_Cheapest_Rank,ordered=TRUE, levels=c("1","2","3"))
Data_Daily$One_Way_Medium_Rank=factor(Data_Daily$One_Way_Medium_Rank, ordered=TRUE,levels=c("1","2","3"))
Data_Daily$Free_Floating_High_price=factor(Data_Daily$Free_Floating_High_price,ordered=TRUE, levels=c("1","2","3"))
Data_Daily$Per_Day_40=factor(Data_Daily$Per_Day_40, ordered=TRUE,levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data_Daily$Per_Day_50=factor(Data_Daily$Per_Day_50, ordered=TRUE,levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data_Daily$Per_Day_60=factor(Data_Daily$Per_Day_60, ordered=TRUE,levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data_Daily$Per_Day_70=factor(Data_Daily$Per_Day_70, ordered=TRUE,levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree'))
Data_Daily$Walking_Time_Affect_Carsharing=factor(Data_Daily$Walking_Time_Affect_Carsharing,ordered=TRUE, levels=c("Extremely negative", 'Somewhat negative', 'Neither positive nor negative','Somewhat positive', 'Extremely positive'))
Data_Daily$Carsharing =factor(Data_Daily$Carsharing,ordered=TRUE, levels=c("No", "Maybe", "Yes"))

#Determine Significant attributes
#Creating Contigency table to calculate the Chi-square

# #Vehicle_Ownership_Car
Col_Name=colnames(Data_Daily)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Daily$Vehicle_Ownership_Car,Data_Daily[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Vehicle_Ownership_Car'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Daily/ChiSquare_Vehicle_Ownership_Car.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr
Data_Daily$Student = NULL
Data_Daily$Citizen = NULL
Data_Daily$Driver_License = NULL
Data_Daily$Walk_to_Campus_Minute = NULL
Data_Daily$Weekdays_to_Campus = NULL
Data_Daily$Weekdays_Other_Trips = NULL
Data_Daily$Weekends_Mode = NULL
Data_Daily$Price_Importance = NULL

#Gender
Col_Name=colnames(Data_Daily)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Daily$Gender,Data_Daily[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Gender'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Daily/ChiSquare_Gender.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
  {
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr
Data_Daily$Income = NULL
Data_Daily$Vehicle_Ownership_Motorbike = NULL
Data_Daily$Per_Day_70 = NULL


#Vehicle_Ownership_bicycle
Col_Name=colnames(Data_Daily)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Daily$Vehicle_Ownership_Bicycle,Data_Daily[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Vehicle_Ownership_Bicycle'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Daily/ChiSquare_Vehicle_Ownership_Bicycle.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr

#TTT_to_Campus_Regular_Mode
Col_Name=colnames(Data_Daily)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Daily$TTT_to_Campus_Regular_Mode,Data_Daily[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'TTT_to_Campus_Regular_Mode'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Daily/ChiSquare_TTT_to_Campus_Regular_Mode.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr

#Travel_Time_Importance
Col_Name=colnames(Data_Daily)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Daily$Travel_Time_Importance,Data_Daily[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Travel_Time_Importance'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Daily/ChiSquare_Travel_Time_Importance.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr
Data_Daily$Convenience_Importance = NULL
Data_Daily$Per_Day_40 = NULL

#Environmentally_Friendly_Importance
Col_Name=colnames(Data_Daily)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Daily$Environmentally_Friendly_Importance,Data_Daily[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Environmentally_Friendly_Importance'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Daily/ChiSquare_Environmentally_Friendly_Importance.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr


#Heard_Carsharing
Col_Name=colnames(Data_Daily)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Daily$Heard_Carsharing,Data_Daily[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Heard_Carsharing'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Daily/ChiSquare_Heard_Carsharing.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr

#Two_Way_Carsharing_Cheapest_Rank
Col_Name=colnames(Data_Daily)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Daily$Two_Way_Carsharing_Cheapest_Rank,Data_Daily[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Two_Way_Carsharing_Cheapest_Rank'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Daily/ChiSquare_Two_Way_Carsharing_Cheapest_Rank.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr
Data_Daily$One_Way_Medium_Rank = NULL
Data_Daily$Free_Floating_High_price = NULL

#Per_Day_50
Col_Name=colnames(Data_Daily)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Daily$Per_Day_50,Data_Daily[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Per_Day_50'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Daily/ChiSquare_Per_Day_50.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr
Data_Daily$Per_Day_60 = NULL


write.csv(Data_Daily, '/Users/samiraziyadidegan/Burris-Carsharing/Daily/Data_DailyـRemoved.csv')
#Data_Daily_Removed = as.data.frame(read_csv("/Users/samiraziyadidegan/Burris-Carsharing/Daily/Data_DailyـRemoved.csv"))


Daily_Model=polr(Carsharing~. , data=Data_Daily, Hess=TRUE)
(Table_Daily <- coef(summary(Daily_Model)))
(p_Daily <- pnorm(abs(Table_Daily[, "t value"]), lower.tail = FALSE) * 2)
(Table_Daily=cbind(Table_Daily, P_Value=p_Daily))
