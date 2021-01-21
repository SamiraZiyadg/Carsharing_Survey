library(readr)
library(MASS)
library(tidyverse)
library(data.table)
library(dplyr)

#Read the files
Data_Daily = as.data.frame(read_csv("/Users/samiraziyadidegan/Burris-Carsharing/Daily/Data_Daily.csv"))

Data_Daily$Gender = factor(Data_Daily$Gender)
Data_Daily$Education = factor(Data_Daily$Education)
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
#Nominal Data: Chi-Squared test
#Creating Contigency table to calculate the Chi-square
#If p_value < 0.01, we can say that attributes are statistically associated and should be kept.
#For example, you could use a Kruskal-Wallis H test to understand whether income differed based on carsharing levels

Col_Name1=c("Gender", "Citizen", "Education", "Driver_License","Vehicle_Ownership_Car","Vehicle_Ownership_Bicycle","Weekdays_to_Campus", "Weekdays_Other_Trips","Vehicle_Ownership_Motorbike","Heard_Carsharing","Weekends_Mode")
CHI_test = data.frame(First=as.character(), Second = as.character(), P_Value=as.numeric())
for (n in 1:length(Col_Name1)) {
  Table = table(Data_Daily$Carsharing,Data_Daily[,n])
  CHI=chisq.test(as.matrix(Table))
  CHI_test[n,1] = 'Carsharing'
  CHI_test[n,2] = Col_Name1[n]
  CHI_test[n,3] = CHI$p.value
}
Attr=NULL
for (n in 1:nrow(CHI_test)) 
{
  if (CHI_test$P_Value[n] >  0.05) {
    Attr = c(Attr,CHI_test$Second[n])
  }
}
Attr

Data_Daily$Gender=NULL
Data_Daily$Citizen=NULL
Data_Daily$Vehicle_Ownership_Car=NULL
Data_Daily$Weekdays_to_Campus=NULL
Data_Daily$Weekdays_Other_Trips=NULL
Data_Daily$Weekends_Mode=NULL


#Ordinal Variables

#P<0.05 means that the variable is significant.

Col_Name2=c("Income", "TTT_to_Campus_Regular_Mode", "Walk_to_Campus_Minute", "Travel_Time_Importance","Price_Importance","Convenience_Importance","Environmentally_Friendly_Importance",
           "Two_Way_Carsharing_Cheapest_Rank","One_Way_Medium_Rank","Free_Floating_High_price","Per_Day_40","Per_Day_50","Per_Day_60","Per_Day_70","Walking_Time_Affect_Carsharing")
Krus_Test = data.frame(First=as.character(), Second = as.character(), P_Value=as.numeric())
for (n in 1:length(Col_Name2)) {
  Krus = kruskal.test(Data_Daily[,n], Data_Daily$Carsharing)
  Krus_Test[n,1] = 'Carsharing'
  Krus_Test[n,2] = Col_Name2[n]
  Krus_Test[n,3] = Krus$p.value
}
Attr=NULL
for (n in 1:nrow(Krus_Test)) 
{
  if (Krus_Test$P_Value[n] >  0.05) {
    Attr = c(Attr,Krus_Test$Second[n])
  }
}
Attr
Data_Daily$Income=NULL
Data_Daily$Walk_to_Campus_Minute=NULL
Data_Daily$Travel_Time_Importance=NULL
Data_Daily$Price_Importance=NULL
Data_Daily$Environmentally_Friendly_Importance=NULL
Data_Daily$Free_Floating_High_price=NULL
Data_Daily$Per_Day_50=NULL
Data_Daily$Per_Day_60=NULL
Data_Daily$Per_Day_70=NULL
Data_Daily$Walking_Time_Affect_Carsharing=NULL


write.csv(Data_Daily, '/Users/samiraziyadidegan/Burris-Carsharing/Daily/Data_DailyـSignificant_Variables.csv')
#Data_Daily_Removed = as.data.frame(read_csv("/Users/samiraziyadidegan/Burris-Carsharing/Daily/Data_DailyـRemoved.csv"))

