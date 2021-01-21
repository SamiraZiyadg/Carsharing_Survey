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
Data_Hourly = as.data.frame(read_csv("/Users/samiraziyadidegan/Burris-Carsharing/Hourly/Data_HourlyـSignificant_Variables.csv"))

Data_Hourly$Education = factor(Data_Hourly$Education)
Data_Hourly$Citizen = factor(Data_Hourly$Citizen)
Data_Hourly$Driver_License = factor(Data_Hourly$Driver_License)
Data_Hourly$Income = factor(Data_Hourly$Income)
Data_Hourly$Vehicle_Ownership_Bicycle = factor(Data_Hourly$Vehicle_Ownership_Bicycle)

Data_Hourly$Walk_to_Campus_Minute = factor(Data_Hourly$Walk_to_Campus_Minute)
Data_Hourly$TTT_to_Campus_Regular_Mode = factor(Data_Hourly$TTT_to_Campus_Regular_Mode)
Data_Hourly$Travel_Time_Importance = factor(Data_Hourly$Travel_Time_Importance)
Data_Hourly$Convenience_Importance = factor(Data_Hourly$Convenience_Importance)
Data_Hourly$Heard_Carsharing = factor(Data_Hourly$Heard_Carsharing)
Data_Hourly$One_Way_Medium_Rank = factor(Data_Hourly$One_Way_Medium_Rank)
Data_Hourly$Free_Floating_High_price = factor(Data_Hourly$Free_Floating_High_price)
Data_Hourly$Per_Hour_8 = factor(Data_Hourly$Per_Hour_8)
Data_Hourly$Per_Hour_10 = factor(Data_Hourly$Per_Hour_10)
Data_Hourly$Walking_Time_Affect_Carsharing = factor(Data_Hourly$Walking_Time_Affect_Carsharing)

Data_Hourly$Carsharing =factor(Data_Hourly$Carsharing,ordered=TRUE, levels=c("No", "Maybe", "Yes"))


Hourly_Model=polr(Carsharing~. , data=Data_Hourly, Hess=TRUE)
(Table_Hourly <- coef(summary(Hourly_Model)))
(p_Hourly <- pnorm(abs(Table_Hourly[, "t value"]), lower.tail = FALSE) * 2)
(Table_Hourly=cbind(Table_Hourly, P_Value=p_Hourly))

(fit1 = polr(Carsharing~., data=Data_Hourly, Hess=TRUE))
fit2 = polr(Carsharing~1 , data=Data_Hourly, Hess=TRUE)

#Stepwise:
#AIC
Hourly_Model_Step_AIC1 = stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2))
Hourly_Model_Step_AIC2 = step(fit2,direction="both",scope=list(upper=fit1,lower=fit2))
#BIC
n = nrow(Data_Hourly)
Hourly_Model_Step_BIC = step(fit2,direction="both",scope=list(upper=fit1,lower=fit2), k = log(n))

#Backward:
#AIC
Hourly_Model_Back_AIC1 = stepAIC(fit1, direction = "backward")
Hourly_Model_Back_AIC2 = step(fit1, direction = "backward")
#BIC
n = nrow(Data_Hourly)
Hourly_Model_Back_BIC = stepAIC(fit1, direction = "backward", k = log(n))

#Forward:
#AIC
Hourly_Model_Forw_AIC1 = stepAIC(fit2,direction="forward",scope=list(upper=fit1,lower=fit2))
Hourly_Model_Forw_AIC2 = step(fit2,direction="forward",scope=list(upper=fit1,lower=fit2))
#BIC
n = nrow(Data_Hourly)
Hourly_Model_Forw_BIC = step(fit2,direction="forward",scope=list(upper=fit1,lower=fit2), k = log(n))

Hourly_Model_AIC=polr(Carsharing~Citizen + Walking_Time_Affect_Carsharing + Per_Hour_8 + 
                        Free_Floating_High_price, data=Data_Hourly, Hess=TRUE)
(Table_Hourly_AIC <- coef(summary(Hourly_Model_AIC)))
(p_Hourly_AIC <- pnorm(abs(Table_Hourly_AIC[, "t value"]), lower.tail = FALSE) * 2)
(Table_Hourly_AIC=cbind(Table_Hourly_AIC, P_Value=p_Hourly_AIC))

Hourly_Model_BIC=polr(Carsharing~Citizen + Walking_Time_Affect_Carsharing , data=Data_Hourly, Hess=TRUE)
(Table_Hourly_BIC <- coef(summary(Hourly_Model_BIC)))
(p_Hourly_BIC <- pnorm(abs(Table_Hourly_BIC[, "t value"]), lower.tail = FALSE) * 2)
(Table_Hourly_BIC=cbind(Table_Hourly_BIC, P_Value=p_Hourly_BIC))


#Comparing
#AIC
extractAIC(Hourly_Model) # returns both p and AIC
extractAIC(Hourly_Model_Step_AIC1)
extractAIC(Hourly_Model_Step_BIC)

#Likelihood
Anova(Hourly_Model, type = 3)
Anova(Hourly_Model_AIC, type = 3)
Anova(Hourly_Model_BIC, type = 3)

Hourly_Model_2=polr(Carsharing~Citizen + Walking_Time_Affect_Carsharing + Per_Hour_8 , data=Data_Hourly, Hess=TRUE)
(Table_Hourly_2 <- coef(summary(Hourly_Model_2)))
(p_Hourly_2 <- pnorm(abs(Table_Hourly_2[, "t value"]), lower.tail = FALSE) * 2)
(Table_Hourly_2=cbind(Table_Hourly_2, P_Value=p_Hourly_2))

Anova(Hourly_Model_2, type = 3)
extractAIC(Hourly_Model_2)



#Likelihood ratio test
#Under that significance level, we would reject
#the null hypothesis and conclude that we should use the more complex model.
lrtest(Hourly_Model, Hourly_Model_AIC)
lrtest(Hourly_Model_2, Hourly_Model_AIC)
lrtest(Hourly_Model_BIC, Hourly_Model_2)

#Hourly_Model_2 is chosen
(ci <- confint(Hourly_Model_2)) # default method gives profiled CIs
confint.default(Hourly_Model_2) # CIs assuming normality

## odds ratios
exp(coef(Hourly_Model_2))
## OR and CI
exp(cbind(Coef = coef(Hourly_Model_2), ci))
Hourly_Model_2
Table_Hourly_2
