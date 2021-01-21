library(readr)
library(MASS)
library(tidyverse)
library(data.table)
library(dplyr)

#Read the files
Data_Hourly = as.data.frame(read_csv("/Users/samiraziyadidegan/Burris-Carsharing/Hourly/Data_Hourly.csv"))

Data_Hourly$Gender = factor(Data_Hourly$Gender)
Data_Hourly$Student = factor(Data_Hourly$Student)
Data_Hourly$Citizen = factor(Data_Hourly$Citizen)
Data_Hourly$Driver_License = factor(Data_Hourly$Driver_License)
Data_Hourly$Vehicle_Ownership_Car = factor(Data_Hourly$Vehicle_Ownership_Car)
Data_Hourly$Vehicle_Ownership_Bicycle = factor(Data_Hourly$Vehicle_Ownership_Bicycle)
Data_Hourly$Vehicle_Ownership_Motorbike = factor(Data_Hourly$Vehicle_Ownership_Motorbike)
Data_Hourly$Weekdays_to_Campus = factor(Data_Hourly$Weekdays_to_Campus)
Data_Hourly$Weekdays_Other_Trips = factor(Data_Hourly$Weekdays_Other_Trips)
Data_Hourly$Weekends_Mode = factor(Data_Hourly$Weekends_Mode)
Data_Hourly$Heard_Carsharing = factor(Data_Hourly$Heard_Carsharing)

#Ordered
Data_Hourly$Income=factor(Data_Hourly$Income, ordered=TRUE, levels=c("Less_than_1500", '1500-2500', 'More_than_2500', "Prefer_not_to_say"))
Data_Hourly$TTT_to_Campus_Regular_Mode=factor(Data_Hourly$TTT_to_Campus_Regular_Mode, ordered=TRUE,levels=c('5 minutes','10 minutes','15 minutes','20 minutes','25 minutes','30 minutes','30+ minutes'))
Data_Hourly$Walk_to_Campus_Minute=factor(Data_Hourly$Walk_to_Campus_Minute,ordered=TRUE, levels=c('5 minutes','10 minutes','15 minutes','20 minutes','25 minutes','30 minutes','30+ minutes'))
Data_Hourly$Travel_Time_Importance=factor(Data_Hourly$Travel_Time_Importance, ordered=TRUE,levels=c('Not at all important', 'Slightly important', 'Moderately important','Very Important', 'Extremely important'))
Data_Hourly$Price_Importance=factor(Data_Hourly$Price_Importance, ordered=TRUE,levels=c('Not at all important', 'Slightly important', 'Moderately important','Very Important', 'Extremely important'))
Data_Hourly$Convenience_Importance=factor(Data_Hourly$Convenience_Importance,ordered=TRUE, levels=c('Not at all important', 'Slightly important', 'Moderately important','Very Important', 'Extremely important'))
Data_Hourly$Environmentally_Friendly_Importance=factor(Data_Hourly$Environmentally_Friendly_Importance, ordered=TRUE,levels=c('Not at all important', 'Slightly important', 'Moderately important','Very Important', 'Extremely important'))
Data_Hourly$Two_Way_Carsharing_Cheapest_Rank=factor(Data_Hourly$Two_Way_Carsharing_Cheapest_Rank,ordered=TRUE, levels=c("1","2","3"))
Data_Hourly$One_Way_Medium_Rank=factor(Data_Hourly$One_Way_Medium_Rank, ordered=TRUE,levels=c("1","2","3"))
Data_Hourly$Free_Floating_High_price=factor(Data_Hourly$Free_Floating_High_price,ordered=TRUE, levels=c("1","2","3"))
Data_Hourly$Per_Hour_6=factor(Data_Hourly$Per_Hour_6, ordered=TRUE,levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data_Hourly$Per_Hour_8=factor(Data_Hourly$Per_Hour_8, ordered=TRUE,levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data_Hourly$Per_Hour_10=factor(Data_Hourly$Per_Hour_10, ordered=TRUE,levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data_Hourly$Per_Hour_12=factor(Data_Hourly$Per_Hour_12, ordered=TRUE,levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data_Hourly$Walking_Time_Affect_Carsharing=factor(Data_Hourly$Walking_Time_Affect_Carsharing,ordered=TRUE, levels=c("Extremely negative", 'Somewhat negative', 'Neither positive nor negative','Somewhat positive', 'Extremely positive'))
Data_Hourly$Carsharing =factor(Data_Hourly$Carsharing,ordered=TRUE, levels=c("No", "Maybe", "Yes"))


#Determine Significant attributes
#Creating Contigency table to calculate the Chi-square
#Vehicle_Ownership_Car
Col_Name=colnames(Data_Hourly)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Hourly$Vehicle_Ownership_Car,Data_Hourly[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Vehicle_Ownership_Car'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Hourly/ChiSquare_Vehicle_Ownership_Car.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr
Data_Hourly$Gender = NULL
Data_Hourly$Student = NULL
Data_Hourly$Citizen = NULL
Data_Hourly$Driver_License = NULL
Data_Hourly$Walk_to_Campus_Minute = NULL
Data_Hourly$Weekdays_to_Campus = NULL
Data_Hourly$Weekdays_Other_Trips = NULL
Data_Hourly$Weekends_Mode = NULL
Data_Hourly$Price_Importance = NULL
Data_Hourly$Travel_Time_Importance = NULL
Data_Hourly$Convenience_Importance = NULL


#Income
Col_Name=colnames(Data_Hourly)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Hourly$Income,Data_Hourly[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Income'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Hourly/ChiSquare_Income.csv')
#If p_value < 0.01, we can say that we are 99% sure that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
  {
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr
Data_Hourly$Per_Hour_6 = NULL

#Vehicle_Ownership_bicycle
Col_Name=colnames(Data_Hourly)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Hourly$Vehicle_Ownership_Bicycle,Data_Hourly[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Vehicle_Ownership_Bicycle'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Hourly/ChiSquare_Vehicle_Ownership_Bicycle.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr


#Vehicle_Ownership_Motorbike
Col_Name=colnames(Data_Hourly)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Hourly$Vehicle_Ownership_Motorbike,Data_Hourly[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Vehicle_Ownership_Motorbike'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Hourly/ChiSquare_Vehicle_Ownership_Motorbike.csv')
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
Col_Name=colnames(Data_Hourly)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Hourly$TTT_to_Campus_Regular_Mode,Data_Hourly[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'TTT_to_Campus_Regular_Mode'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Hourly/ChiSquare_TTT_to_Campus_Regular_Mode.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr


#Environmentally_Friendly_Importance
Col_Name=colnames(Data_Hourly)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Hourly$Environmentally_Friendly_Importance,Data_Hourly[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Environmentally_Friendly_Importance'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Hourly/ChiSquare_Environmentally_Friendly_Importance.csv')
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
Col_Name=colnames(Data_Hourly)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Hourly$Heard_Carsharing,Data_Hourly[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Heard_Carsharing'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Hourly/ChiSquare_Heard_Carsharing.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr
Data_Hourly$Free_Floating_High_price = NULL


#Two_Way_Carsharing_Cheapest_Rank
Col_Name=colnames(Data_Hourly)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Hourly$Two_Way_Carsharing_Cheapest_Rank,Data_Hourly[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Two_Way_Carsharing_Cheapest_Rank'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Hourly/ChiSquare_Two_Way_Carsharing_Cheapest_Rank.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr
Data_Hourly$One_Way_Medium_Rank = NULL


#Per_Hour_8
Col_Name=colnames(Data_Hourly)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Hourly$Per_Hour_8,Data_Hourly[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Per_Hour_8'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Hourly/ChiSquare_Per_Hour_8.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr
Data_Hourly$Per_Hour_10 = NULL
Data_Hourly$Per_Hour_12 = NULL

#Walking_Time_Affect_Carsharing
Col_Name=colnames(Data_Hourly)
P_Value = data.frame(First=as.character(), Second = as.character(), PValue=as.numeric())
for (n in 1:length(Col_Name)) {
  Table = table(Data_Hourly$Walking_Time_Affect_Carsharing,Data_Hourly[,n])
  CHI=chisq.test(as.matrix(Table))
  P_Value[n,1] = 'Walking_Time_Affect_Carsharing'
  P_Value[n,2] = Col_Name[n]
  P_Value[n,3] = CHI$p.value
}
write.csv(P_Value, '/Users/samiraziyadidegan/Burris-Carsharing/Hourly/ChiSquare_Walking_Time_Affect_Carsharing.csv')
#If p_value < 0.01, we can say that attributes are statistically associated and should be removed
Attr=NULL
for (n in 1:nrow(P_Value)) 
{
  if (P_Value$PValue[n] < 0.01) {
    Attr = c(Attr,P_Value$Second[n])
  }
}
Attr

write.csv(Data_Hourly, '/Users/samiraziyadidegan/Burris-Carsharing/Hourly/Data_HourlyـRemoved.csv')
#Data_Hourly_Removed = as.data.frame(read_csv("/Users/samiraziyadidegan/Burris-Carsharing/Hourly/Data_HourlyـRemoved.csv"))

Hourly_Model=polr(Carsharing~. , data=Data_Hourly, Hess=TRUE)
Table_Hourly <- coef(summary(Hourly_Model))
p_Hourly <- pnorm(abs(Table_Hourly[, "t value"]), lower.tail = FALSE) * 2
(Table_Hourly=cbind(Table_Hourly, P_Value=p_Hourly))
