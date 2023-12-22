############################## Install packages ##############################

install.packages("survival")
install.packages("ggfortify")
install.packages("survminer")
library(survminer)
library("stats")
library("survival")
library("ggplot2")
library("ggfortify")
library(dplyr)
library(stats)
library(xtable)

################################ Read the data ################################

df = read.csv("stroke_rtw.csv")
View(df)
str(df)

############################### Format the data ###############################

# Change relevant variables to categorical data
df$work_status_pre = as.factor(df$work_status_pre)
df$sex = as.factor(df$sex)
df$region = as.factor(df$region)
df$stroke_severity = as.factor(df$stroke_severity)
df$alloc = as.factor(df$alloc)

# Change the format of dates
df$rtw_dte = as.Date(df$rtw_dte)
df$randomisation_dte = as.Date(df$randomisation_dte)
days = df$rtw_dte - df$randomisation_dte
df$time_diff = as.integer(days)
str(df)

# Change Usual Care to be the reference category
df$alloc = factor(df$alloc, levels = c("Usual Care", "ESSVR"))

#################################### Aim 1 ####################################

# Subset the data for the ESSVR arm
aim1 = subset(df, alloc == 'ESSVR')

# Aim 1.1 
# Chi squared significant:
aim1.1 = table(aim1$work_status_pre, aim1$essvr_complete_flg)
chisq.test(aim1.1)

# Aim 1.2
# T test not significant:
t.test(aim1$hpw_pre ~ aim1$essvr_complete_flg, alternative = "two.sided")

# Aim 1.3
# Chi squared significant 
aim1.3 = table(aim1$stroke_severity, aim1$essvr_complete_flg)
chisq.test(aim1.3) 

# Logistic regression with covariates:
aim1_logistic = glm(essvr_complete_flg ~ work_status_pre + hpw_pre + stroke_severity + sex + age, family = binomial, aim1) 
summary(aim1_logistic)

# Stratified by sex
aim1_males = subset(aim1, sex == "Male")
aim1_females = subset(aim1, sex == "Female")

aim1_logistic_males = glm(essvr_complete_flg ~ work_status_pre + hpw_pre + stroke_severity + age, family = binomial, data = aim1_males)
summary(aim1_logistic_males) 
aim1_logistic_females = glm(essvr_complete_flg ~ work_status_pre + hpw_pre + stroke_severity + age, family = binomial, data = aim1_females)
summary(aim1_logistic_females) 

print(xtable(aim1_logistic, type = "latex"), file = "logistic1.tex")
print(xtable(aim1_logistic_males, type = "latex"), file = "logisticm.tex")
print(xtable(aim1_logistic_females, type = "latex"), file = "logisticf.tex")

################################## Aim 2 ####################################

## Test how the programme affects the time taken to return to work:

# Cox proportional hazard model: 
aim2.1 = coxph(formula = Surv(time_diff, rtw_flg) ~ alloc + stroke_severity + sex + age, data = df)
summary(aim2.1)

print(xtable(aim2.1, type = "latex"), file = "cox1.tex")


# Plot Kaplan-Meier curve:
graph = survfit(Surv(time_diff, rtw_flg) ~ alloc, df)
autoplot(graph, xlab = "time (days)", ylab = "Probability of staying unemployed", main = "Unemployment probability over time")

# Stratify by sex:
aim2.2 = coxph(formula = Surv(time_diff, rtw_flg) ~ alloc + stroke_severity + age, data = males)
summary(aim2.2)
ggforest(aim2.2, data = time_split, main = "Return to work rate - males")

aim2.3 = coxph(formula = Surv(time_diff, rtw_flg) ~ alloc + stroke_severity + age, data = females)
summary(aim2.3)
ggforest(aim2.3, data = time_split, main = "Return to work rate - females")

# Time-split Cox model:
time_cut = 183
time_split = survSplit(Surv(time_diff, rtw_flg) ~ ., data = df,
                       cut = time_cut,
                       end = "tstop",
                       episode = "rtw_group")

time_split[, "rtw_group"] = factor(time_split[, "rtw_group"])

cox_time_split = coxph(Surv(tstart, tstop, event = rtw_flg) ~ alloc*rtw_group + age + sex + 
                         stroke_severity, data = time_split)
summary(cox_time_split)
ggforest(cox_time_split, data = time_split)


################################## Aim 3 ####################################

# Linear regression with covariates:
aim3 = glm(health_score ~ alloc + age + stroke_severity + sex, family = gaussian, df)
summary(aim3) 
print(xtable(aim3, type = "latex"), file = "3.tex")

# Stratify by sex:
aim3_males = glm(health_score ~ alloc + age + stroke_severity, family = gaussian, males)
summary(aim3_males) 
aim3_females = glm(health_score ~ alloc + age + stroke_severity, family = gaussian, females)
summary(aim3_females) 


############################################################################### 






