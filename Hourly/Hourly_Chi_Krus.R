library(readr)
library(MASS)
library(tidyverse)
library(data.table)
library(dplyr)

#Read the files
Data_Hourly = as.data.frame(read_csv("/Users/samiraziyadidegan/Burris-Carsharing/Hourly/Data_Hourly.csv"))

Data_Hourly$Gender = factor(Data_Hourly$Gender)
Data_Hourly$Education = factor(Data_Hourly$Education)
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
#Nominal Data: Chi-Squared test
#Creating Contigency table to calculate the Chi-square
#If p_value < 0.05, we can say that attributes are statistically associated and should be kept.
#For example, you could use a Kruskal-Wallis H test to understand whether income differed based on carsharing levels

Col_Name1=c("Gender", "Citizen", "Education", "Driver_License","Vehicle_Ownership_Car","Vehicle_Ownership_Bicycle","Weekdays_to_Campus", "Weekdays_Other_Trips","Vehicle_Ownership_Motorbike","Heard_Carsharing","Weekends_Mode")
CHI_test = data.frame(First=as.character(), Second = as.character(), P_Value=as.numeric(), CHI=as.numeric())
for (n in 1:length(Col_Name1)) {
  Table = table(Data_Hourly$Carsharing,Data_Hourly[,n])
  CHI=chisq.test(as.matrix(Table))
  CHI_test[n,1] = 'Carsharing'
  CHI_test[n,2] = Col_Name1[n]
  CHI_test[n,3] = CHI$p.value
  CHI_test[n,4] = CHI$statistic
}
Attr=NULL
for (n in 1:nrow(CHI_test)) 
{
  if (CHI_test$P_Value[n] >  0.05) {
    Attr = c(Attr,CHI_test$Second[n])
  }
}
Attr

Data_Hourly$Gender=NULL
Data_Hourly$Vehicle_Ownership_Car=NULL
Data_Hourly$Weekdays_to_Campus=NULL
Data_Hourly$Weekdays_Other_Trips=NULL
Data_Hourly$Vehicle_Ownership_Motorbike=NULL
Data_Hourly$Weekends_Mode=NULL


#Ordinal Variables

#P<0.05 means that the variable is significant.

Col_Name2=c("Income", "TTT_to_Campus_Regular_Mode", "Walk_to_Campus_Minute", "Travel_Time_Importance","Price_Importance","Convenience_Importance","Environmentally_Friendly_Importance",
           "Two_Way_Carsharing_Cheapest_Rank","One_Way_Medium_Rank","Free_Floating_High_price","Per_Hour_6","Per_Hour_8","Per_Hour_10","Per_Hour_12","Walking_Time_Affect_Carsharing")
Krus_Test = data.frame(First=as.character(), Second = as.character(), P_Value=as.numeric())
for (n in 1:length(Col_Name2)) {
  Krus = kruskal.test(Data_Hourly[,n], Data_Hourly$Carsharing)
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
Data_Hourly$Price_Importance=NULL
Data_Hourly$Environmentally_Friendly_Importance=NULL
Data_Hourly$Two_Way_Carsharing_Cheapest_Rank=NULL
Data_Hourly$Per_Hour_6=NULL
Data_Hourly$Per_Hour_12=NULL


write.csv(Data_Hourly, '/Users/samiraziyadidegan/Burris-Carsharing/Hourly/Data_HourlyـSignificant_Variables.csv')

