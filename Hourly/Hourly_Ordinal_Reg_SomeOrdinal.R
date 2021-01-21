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
Data_Hourly = as.data.frame(read_csv("/Users/samiraziyadidegan/Burris-Carsharing/Hourly/Data_HourlyـRemoved.csv"))

Data_Hourly$Vehicle_Ownership_Car = factor(Data_Hourly$Vehicle_Ownership_Car)
Data_Hourly$Vehicle_Ownership_Bicycle = factor(Data_Hourly$Vehicle_Ownership_Bicycle)
Data_Hourly$Vehicle_Ownership_Motorbike = factor(Data_Hourly$Vehicle_Ownership_Motorbike)
Data_Hourly$Heard_Carsharing = factor(Data_Hourly$Heard_Carsharing)
#Ordered
Data_Hourly$Income=factor(Data_Hourly$Income, ordered=TRUE, levels=c("Less_than_1500", '1500-2500', 'More_than_2500', "Prefer_not_to_say"))
Data_Hourly$TTT_to_Campus_Regular_Mode=factor(Data_Hourly$TTT_to_Campus_Regular_Mode, ordered=TRUE,levels=c('5 minutes','10 minutes','15 minutes','20 minutes','25 minutes','30 minutes','30+ minutes'))
Data_Hourly$Environmentally_Friendly_Importance=factor(Data_Hourly$Environmentally_Friendly_Importance, ordered=TRUE,levels=c('Not at all important', 'Slightly important', 'Moderately important','Very Important', 'Extremely important'))
Data_Hourly$Two_Way_Carsharing_Cheapest_Rank=factor(Data_Hourly$Two_Way_Carsharing_Cheapest_Rank,ordered=TRUE, levels=c("1","2","3"))
Data_Hourly$Per_Hour_8=factor(Data_Hourly$Per_Hour_8, ordered=TRUE,levels=c('Strongly disagree', 'Disagree', 'Neither agree nor disagree','Agree', 'Strongly Agree'))
Data_Hourly$Walking_Time_Affect_Carsharing=factor(Data_Hourly$Walking_Time_Affect_Carsharing,ordered=TRUE, levels=c("Extremely negative", 'Somewhat negative', 'Neither positive nor negative','Somewhat positive', 'Extremely positive'))
Data_Hourly$Carsharing =factor(Data_Hourly$Carsharing,ordered=TRUE, levels=c("No", "Maybe", "Yes"))


Hourly_Model=polr(Carsharing~. , data=Data_Hourly, Hess=TRUE)
(Table_Hourly <- coef(summary(Hourly_Model)))
(p_Hourly <- pnorm(abs(Table_Hourly[, "t value"]), lower.tail = FALSE) * 2)
(Table_Hourly=cbind(Table_Hourly, P_Value=p_Hourly))

fit1 = polr(Carsharing~., data=Data_Hourly, Hess=TRUE)
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

Hourly_Model_AIC=polr(Carsharing~Vehicle_Ownership_Car + Environmentally_Friendly_Importance + 
                        Walking_Time_Affect_Carsharing + Per_Hour_8 , data=Data_Hourly, Hess=TRUE)
(Table_Hourly_AIC <- coef(summary(Hourly_Model_AIC)))
(p_Hourly_AIC <- pnorm(abs(Table_Hourly_AIC[, "t value"]), lower.tail = FALSE) * 2)
(Table_Hourly_AIC=cbind(Table_Hourly_AIC, P_Value=p_Hourly_AIC))

Hourly_Model_BIC=polr(Carsharing~Vehicle_Ownership_Car + Environmentally_Friendly_Importance , data=Data_Hourly, Hess=TRUE)
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

#Likelihood ratio test
#Under that significance level, we would reject
#the null hypothesis and conclude that we should use the more complex model.
lrtest(Hourly_Model, Hourly_Model_AIC)
lrtest(Hourly_Model_BIC, Hourly_Model_AIC)

#Hourly_Model_AIC is chosen
(ci <- confint(Hourly_Model_AIC)) # default method gives profiled CIs
confint.default(Hourly_Model_AIC) # CIs assuming normality

## odds ratios
exp(coef(Hourly_Model_AIC))
## OR and CI
exp(cbind(OR = coef(Hourly_Model_AIC), ci))

