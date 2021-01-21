library(readr)
library(MASS)
library(tidyverse)
library(data.table)
library(dplyr)
library(leaps)
library(caret)
library(lmtest)
library(car)


#Read the files
Data_Daily = as.data.frame(read_csv("/Users/samiraziyadidegan/Burris-Carsharing/Daily/Data_DailyـSignificant_Variables.csv"))

Data_Daily$Education = factor(Data_Daily$Education)
Data_Daily$Driver_License = factor(Data_Daily$Driver_License)
Data_Daily$Vehicle_Ownership_Bicycle = factor(Data_Daily$Vehicle_Ownership_Bicycle)
Data_Daily$Vehicle_Ownership_Motorbike = factor(Data_Daily$Vehicle_Ownership_Motorbike)
Data_Daily$Heard_Carsharing = factor(Data_Daily$Heard_Carsharing)

#Ordered
Data_Daily$TTT_to_Campus_Regular_Mode=factor(Data_Daily$TTT_to_Campus_Regular_Mode)
Data_Daily$Convenience_Importance=factor(Data_Daily$Convenience_Importance)
Data_Daily$Two_Way_Carsharing_Cheapest_Rank=factor(Data_Daily$Two_Way_Carsharing_Cheapest_Rank)
Data_Daily$One_Way_Medium_Rank=factor(Data_Daily$One_Way_Medium_Rank)
Data_Daily$Per_Day_40=factor(Data_Daily$Per_Day_40)

Data_Daily$Carsharing =factor(Data_Daily$Carsharing,ordered=TRUE, levels=c("No", "Maybe", "Yes"))

Daily_Model=polr(Carsharing~. , data=Data_Daily, Hess=TRUE)
(Table_Daily <- coef(summary(Daily_Model)))
(p_Daily <- pnorm(abs(Table_Daily[, "t value"]), lower.tail = FALSE) * 2)
(Table_Daily=cbind(Table_Daily, P_Value=p_Daily))

fit1 = polr(Carsharing~., data=Data_Daily, Hess=TRUE)
fit2 = polr(Carsharing~1 , data=Data_Daily, Hess=TRUE)

#Stepwise:
#AIC
Daily_Model_Step_AIC1 = stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2))
Daily_Model_Step_AIC2 = step(fit2,direction="both",scope=list(upper=fit1,lower=fit2))
#BIC
n = nrow(Data_Daily)
Daily_Model_Step_BIC = step(fit2,direction="both",scope=list(upper=fit1,lower=fit2), k = log(n))

#Backward:
#AIC
Daily_Model_Back_AIC1 = stepAIC(fit1, direction = "backward")
Daily_Model_Back_AIC2 = step(fit1, direction = "backward")
#BIC
n = nrow(Data_Daily)
Daily_Model_Back_BIC = stepAIC(fit1, direction = "backward", k = log(n))

#Forward:
#AIC
Daily_Model_Forw_AIC1 = stepAIC(fit2,direction="forward",scope=list(upper=fit1,lower=fit2))
Daily_Model_Forw_AIC2 = step(fit2,direction="forward",scope=list(upper=fit1,lower=fit2))
#BIC
n = nrow(Data_Daily)
Daily_Model_Forw_BIC = step(fit2,direction="forward",scope=list(upper=fit1,lower=fit2), k = log(n))

Daily_Model_AIC=polr(Carsharing~Per_Day_40 + Driver_License + Two_Way_Carsharing_Cheapest_Rank + 
                       One_Way_Medium_Rank + Convenience_Importance + Education + 
                       Vehicle_Ownership_Motorbike, data=Data_Daily, Hess=TRUE)
(Table_Daily_AIC <- coef(summary(Daily_Model_AIC)))
(p_Daily_AIC <- pnorm(abs(Table_Daily_AIC[, "t value"]), lower.tail = FALSE) * 2)
(Table_Daily_AIC=cbind(Table_Daily_AIC, P_Value=p_Daily_AIC))

Daily_Model_BIC=polr(Carsharing~Driver_License , data=Data_Daily, Hess=TRUE)
(Table_Daily_BIC <- coef(summary(Daily_Model_BIC)))
(p_Daily_BIC <- pnorm(abs(Table_Daily_BIC[, "t value"]), lower.tail = FALSE) * 2)
(Table_Daily_BIC=cbind(Table_Daily_BIC, P_Value=p_Daily_BIC))


#Comparing
#AIC
extractAIC(Daily_Model) # returns both p and AIC
extractAIC(Daily_Model_Step_AIC1)
extractAIC(Daily_Model_Step_BIC)

#Likelihood
Anova(Daily_Model, type = 3)
Anova(Daily_Model_AIC, type = 3)
Anova(Daily_Model_BIC, type = 3)

#Remove Vehicle_Ownership_Motorbike
Daily_Model_2=polr(Carsharing~Per_Day_40 + Driver_License + Two_Way_Carsharing_Cheapest_Rank + 
                      One_Way_Medium_Rank + Convenience_Importance + Education,data=Data_Daily, Hess=TRUE)
(Table_Daily_2 <- coef(summary(Daily_Model_2)))
(p_Daily_2 <- pnorm(abs(Table_Daily_2[, "t value"]), lower.tail = FALSE) * 2)
(Table_Daily_2=cbind(Table_Daily_2, P_Value=p_Daily_2))

Anova(Daily_Model_2, type = 3)
extractAIC(Daily_Model_2)


#Remove Education
Daily_Model_3=polr(Carsharing~Per_Day_40 + Driver_License + Two_Way_Carsharing_Cheapest_Rank + 
                     One_Way_Medium_Rank + Convenience_Importance,data=Data_Daily, Hess=TRUE)
(Table_Daily_3 <- coef(summary(Daily_Model_3)))
(p_Daily_3 <- pnorm(abs(Table_Daily_3[, "t value"]), lower.tail = FALSE) * 2)
(Table_Daily_3=cbind(Table_Daily_3, P_Value=p_Daily_3))

Anova(Daily_Model_3, type = 3)
extractAIC(Daily_Model_3)


#Remove Convenience_Importance
Daily_Model_4=polr(Carsharing~Per_Day_40 + Driver_License + Two_Way_Carsharing_Cheapest_Rank + 
                     One_Way_Medium_Rank ,data=Data_Daily, Hess=TRUE)
(Table_Daily_4 <- coef(summary(Daily_Model_4)))
(p_Daily_4 <- pnorm(abs(Table_Daily_4[, "t value"]), lower.tail = FALSE) * 2)
(Table_Daily_4=cbind(Table_Daily_4, P_Value=p_Daily_4))

Anova(Daily_Model_4, type = 3)
extractAIC(Daily_Model_4)


#Likelihood ratio test
#Under that significance level, we would reject
#the null hypothesis and conclude that we should use the more complex model.
lrtest(Daily_Model, Daily_Model_AIC)
lrtest(Daily_Model_2, Daily_Model_AIC)
lrtest(Daily_Model_3, Daily_Model_2)
lrtest(Daily_Model_4, Daily_Model_3)
lrtest(Daily_Model_BIC, Daily_Model_4)

#Daily_Model_3 is chosen
(ci <- confint(Daily_Model_3)) # default method gives profiled CIs
confint.default(Daily_Model_3) # CIs assuming normality

## odds ratios
exp(coef(Daily_Model_3))
## OR and CI
exp(cbind(OR = coef(Daily_Model_3), ci))
Daily_Model_3
Table_Daily_3



