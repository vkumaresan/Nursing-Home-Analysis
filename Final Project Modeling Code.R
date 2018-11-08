# Final Project Modeling Code


# Long Stay
### 410: Percentage of long-stay residents experiencing one or more falls with major injury 
total_410 <- total[which(total$Measure.Code == '410'),]

reg_410 = lm(Four.Quarter.Average.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_410)
summary(reg_410)

# Log Transformed
total_410 <- total_410[which(total_410$Four.Quarter.Average.Score != 0),]
reg_410 = lm(log(Four.Quarter.Average.Score) ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_410)
summary(reg_410)

# Try Box-Cox transformation 
library(MASS)
box_cox_410 <- boxcox(Four.Quarter.Average.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_410)
trans <- box_cox_410$x[which.max(box_cox_410$y)]

reg_410_bc = lm(((Four.Quarter.Average.Score^trans-1)/trans) ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_410)
summary(reg_410_bc) # 0.02505

# Convert outcome to binary outcome
# 1: Greater than pop average, 0 otherwise
total_410 <- total[which(total$Measure.Code == '410'),]
total_410 <- na.omit(total_410)

mean(total_410$Four.Quarter.Average.Score)
total_410$AdjScoreFlag <- 0

total_410$AdjScoreFlag[total_410$Four.Quarter.Average.Score > (mean(total_410$Four.Quarter.Average.Score))] <- 1
logreg_410 = glm(AdjScoreFlag ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, family = binomial, data= total_410)
summary(logreg_410)

predprobs = predict(logreg_410, type = "response")
rawresids = total_410$AdjScoreFlag - predprobs


threshold = 0.5
table(total_410$AdjScoreFlag, logreg_410$fitted > threshold)


library("pROC")
roc(total_410$AdjScoreFlag, fitted(logreg_410), plot=T, legacy.axes=T) # 0.5932

### 402: Percentage of long-stay residents who self-report moderate to severe pain 
total_402 <- total[which(total$Measure.Code == '402'),]

reg_402 = lm(Four.Quarter.Average.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_402)
summary(reg_402)

# Log Transformed
total_402 <- total_402[which(total_402$Four.Quarter.Average.Score != 0),]
reg_402 = lm(log(Four.Quarter.Average.Score) ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_402)
summary(reg_402)

# Try Box-Cox transformation 
library(MASS)
box_cox_402 <- boxcox(Four.Quarter.Average.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_402)
trans <- box_cox_402$x[which.max(box_cox_402$y)]

reg_402_bc = lm(((Four.Quarter.Average.Score^trans-1)/trans) ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_402)
summary(reg_402_bc) # 0.06627

# Convert outcome to binary outcome
# 1: Greater than pop average, 0 otherwise
total_402 <- total[which(total$Measure.Code == '402'),]
total_402 <- na.omit(total_402)

mean(total_402$Four.Quarter.Average.Score)
total_402$AdjScoreFlag <- 0

total_402$AdjScoreFlag[total_402$Four.Quarter.Average.Score > (mean(total_402$Four.Quarter.Average.Score))] <- 1
logreg_402 = glm(AdjScoreFlag ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, family = binomial, data= total_402)
summary(logreg_402)

predprobs = predict(logreg_402, type = "response")
rawresids = total_402$AdjScoreFlag - predprobs


threshold = 0.5
table(total_402$AdjScoreFlag, logreg_402$fitted > threshold)


library("pROC")
roc(total_402$AdjScoreFlag, fitted(logreg_402), plot=T, legacy.axes=T) # 0.615

# 551 (Five-Star Rating)
total_551 <- total_five_star[which(total_five_star$Measure.Code == '551'),]
total_551 <- na.omit(total_551)

names(total_five_star)
reg_551 = lm(log(Adjusted.Score^2) ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_551)
summary(reg_551) #0.07023

hist(total_551$Adjusted.Score)
plot(Adjusted.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_551, ask=T)


table(total_551$Adjusted.Score) # only 19 providers have 'zero' values, so we can try to remove them and log-transform values
total_551 <- total_551[which(total_551$Adjusted.Score != 0),]
reg_551 = lm(log(Adjusted.Score) ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_551)
summary(reg_551)

# Convert outcome to binary outcome
# 1: Greater than pop average, 0 otherwise
total_551 <- total_five_star[which(total_five_star$Measure.Code == '551'),]
total_551 <- na.omit(total_551)

mean(total_551$Adjusted.Score)
total_551$AdjScoreFlag <- 0

total_551 <- na.omit(total_551)
total_551$AdjScoreFlag[total_551$Adjusted.Score > (mean(total_551$Adjusted.Score))] <- 1
logreg_551 = glm(AdjScoreFlag ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, family = binomial, data= total_551)
summary(logreg_551)

predprobs = predict(logreg_551, type = "response")
rawresids = total_551$AdjScoreFlag - predprobs


threshold = 0.5
table(total_551$AdjScoreFlag, logreg_551$fitted > threshold)


library("pROC")
roc(total_551$AdjScoreFlag, fitted(logreg_551), plot=T, legacy.axes=T) #0.6271


# Check for multicollinearity
cor(total_551$Total.Weighted.Health.Survey.Score, total_551$Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection)
cor(total_551$Number.of.Health.Deficiencies.on.Survey.Under.New.Process, total_551$Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection)
cor(total_551$Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day, total_551$Adjusted.CNA.Staffing.Hours.per.Resident.per.Day)
cor(total_551$Number.of.Certified.Beds, total_551$Adjusted.RN.Staffing.Hours.per.Resident.per.Day)


# Long Stay
### 424: Percentage of short-stay residents who self-report moderate to severe pain 
total_424 <- total[which(total$Measure.Code == '424'),]

# Plot distribution of outcome variable
hist(total_424$Four.Quarter.Average.Score)

# Test linear regression models for each outcome
reg_424 = lm(Four.Quarter.Average.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds^2 + Number.of.Certified.Beds , data= total_424)
summary(reg_424)

reg_424_min = lm(Four.Quarter.Average.Score ~ Total.Weighted.Health.Survey.Score + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_424)
summary(reg_424_min)

# Test quarters
reg_424_1 = lm(Q1.Measure.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_424)
summary(reg_424_1)

reg_424_2 = lm(Q2.Measure.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_424)
summary(reg_424_2)

reg_424_3 = lm(Q3.Measure.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_424)
summary(reg_424_3)

reg_424_4 = lm(Q4.Measure.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_424)
summary(reg_424_4)

# Small differences among quarter predictions

# Plot predictor variables vs outcome variable
plot(Four.Quarter.Average.Score ~ Total.Weighted.Health.Survey.Score +  Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_424, ask=T)

plot(log(Four.Quarter.Average.Score^2) ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_424, ask=T)
hist(log(total_424$Four.Quarter.Average.Score))

# Try Box-Cox transformation 
library(MASS)
table(total_424$Four.Quarter.Average.Score) 
total_424 <- total_424[which(total_424$Four.Quarter.Average.Score != 0),]
box_cox_424 <- boxcox(Four.Quarter.Average.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_424)
trans <- box_cox_424$x[which.max(box_cox_424$y)]

reg_424_bc = lm(((Four.Quarter.Average.Score^trans-1)/trans) ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_424)
summary(reg_424_bc) # 0.05545

# Convert outcome to binary outcome
# 1: Greater than pop average, 0 otherwise
total_424 <- total[which(total$Measure.Code == '424'),]
total_424 <- na.omit(total_424)

mean(total_424$Four.Quarter.Average.Score)
total_424$AdjScoreFlag <- 0

total_424$AdjScoreFlag[total_424$Four.Quarter.Average.Score > (mean(total_424$Four.Quarter.Average.Score))] <- 1
logreg_424 = glm(AdjScoreFlag ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, family = binomial, data= total_424)
summary(logreg_424)

predprobs = predict(logreg_424, type = "response")
rawresids = total_424$AdjScoreFlag - predprobs


threshold = 0.5
table(total_424$AdjScoreFlag, logreg_424$fitted > threshold)


library("pROC")
roc(total_424$AdjScoreFlag, fitted(logreg_424), plot=T, legacy.axes=T) # 0.6058

# Scorecard (multinomial logistic)
# 1: A = Top 25%, 2: B = 25-50%, 3: C = 50-75%, 4: F = < 75%
# quantile in R uses median
total_424 <- total[which(total$Measure.Code == '424'),]
total_424 <- na.omit(total_424)

total_424$AdjScoreGrade <- 0

total_424$AdjScoreGrade[total_424$Four.Quarter.Average.Score >= (quantile(total_424$Four.Quarter.Average.Score, .75))] <- 1
total_424$AdjScoreGrade[total_424$Four.Quarter.Average.Score >= (quantile(total_424$Four.Quarter.Average.Score, .50)) & total_424$Four.Quarter.Average.Score < (quantile(total_424$Four.Quarter.Average.Score, .75)) ] <- 2
total_424$AdjScoreGrade[total_424$Four.Quarter.Average.Score >= (quantile(total_424$Four.Quarter.Average.Score, .25)) & total_424$Four.Quarter.Average.Score < (quantile(total_424$Four.Quarter.Average.Score, .50)) ] <- 3
total_424$AdjScoreGrade[total_424$Four.Quarter.Average.Score < (quantile(total_424$Four.Quarter.Average.Score, .25))] <- 4

library(nnet)
multinomlogreg_424 = multinom(AdjScoreGrade ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_424)

table(total_424$AdjScoreGrade)

predprobs = fitted(multinomlogreg_424)

rawresid1 = (total_424$AdjScoreGrade == 1) -  predprobs[,1]

multiclass.roc(total_424$AdjScoreGrade, fitted(multinomlogreg_424), plot=T, legacy.axes=T) 

# 523: Percentage of short-stay residents who were successfully discharged to the community (Five-Star Rating)
total_523 <- total_five_star[which(total_five_star$Measure.Code == '523'),]
total_523 <- na.omit(total_523)

# Plot distribution of outcome variable
hist(total_523$Adjusted.Score)

# Test linear regression models for each outcome
reg_523 = lm(Adjusted.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds^2 + Number.of.Certified.Beds , data= total_523)
summary(reg_523) # 0.05102

reg_523_min = lm(Adjusted.Score ~ Total.Weighted.Health.Survey.Score + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_523)
summary(reg_523_min)

# Plot predictor variables vs outcome variable
plot(Adjusted.Score ~ Total.Weighted.Health.Survey.Score +  Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_523, ask=T)

# Convert outcome to binary outcome
# 1: Greater than pop average, 0 otherwise
total_523 <- total_five_star[which(total_five_star$Measure.Code == '523'),]
total_523 <- na.omit(total_523)

mean(total_523$Adjusted.Score)
total_523$AdjScoreFlag <- 0

total_523$AdjScoreFlag[total_523$Adjusted.Score > (mean(total_523$Adjusted.Score))] <- 1
logreg_523 = glm(AdjScoreFlag ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, family = binomial, data= total_523)
summary(logreg_523)

predprobs = predict(logreg_523, type = "response")
rawresids = total_523$AdjScoreFlag - predprobs


threshold = 0.5
table(total_523$AdjScoreFlag, logreg_523$fitted > threshold)


library("pROC")
roc(total_523$AdjScoreFlag, fitted(logreg_523), plot=T, legacy.axes=T) # 0.6424

# Scorecard (multinomial logistic)
# 1: A = Top 25%, 2: B = 25-50%, 3: C = 50-75%, 4: F = < 75%
# quantile in R uses median
total_523 <- total_five_star[which(total_five_star$Measure.Code == '523'),]
total_523 <- na.omit(total_523)

total_523$AdjScoreGrade <- 0

total_523$AdjScoreGrade[total_523$Adjusted.Score >= (quantile(total_523$Adjusted.Score, .75))] <- 1
total_523$AdjScoreGrade[total_523$Adjusted.Score >= (quantile(total_523$Adjusted.Score, .50)) & total_523$Adjusted.Score < (quantile(total_523$Adjusted.Score, .75)) ] <- 2
total_523$AdjScoreGrade[total_523$Adjusted.Score >= (quantile(total_523$Adjusted.Score, .25)) & total_523$Adjusted.Score < (quantile(total_523$Adjusted.Score, .50)) ] <- 3
total_523$AdjScoreGrade[total_523$Adjusted.Score < (quantile(total_523$Adjusted.Score, .25))] <- 4

library(nnet)
multinomlogreg_523 = multinom(AdjScoreGrade ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_523)

table(total_523$AdjScoreGrade)

predprobs = fitted(multinomlogreg_523)

rawresid1 = (total_523$AdjScoreGrade == 1) -  predprobs[,1]

multiclass.roc(total_523$AdjScoreGrade, fitted(multinomlogreg_523), plot=T, legacy.axes=T) 


# 521: Percentage of short-stay residents who were rehospitalized after a nursing home admission total_523 <- total_five_star[which(total_five_star$Measure.Code == '523'),]
total_521 <- total_five_star[which(total_five_star$Measure.Code == '521'),]
total_521 <- na.omit(total_521)

# Plot distribution of outcome variable
hist(total_521$Adjusted.Score)

# Test linear regression models for each outcome
reg_521 = lm(Adjusted.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds^2 + Number.of.Certified.Beds , data= total_521)
summary(reg_521) # 0.01249

reg_521_min = lm(Adjusted.Score ~ Total.Weighted.Health.Survey.Score + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_521)
summary(reg_521_min)

# Plot predictor variables vs outcome variable
plot(Adjusted.Score ~ Total.Weighted.Health.Survey.Score +  Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_521, ask=T)

# Convert outcome to binary outcome
# 1: Greater than pop average, 0 otherwise
total_521 <- total_five_star[which(total_five_star$Measure.Code == '521'),]
total_521 <- na.omit(total_521)

mean(total_521$Adjusted.Score)
total_521$AdjScoreFlag <- 0

total_521$AdjScoreFlag[total_521$Adjusted.Score > (mean(total_521$Adjusted.Score))] <- 1
logreg_521 = glm(AdjScoreFlag ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, family = binomial, data= total_521)
summary(logreg_521)

predprobs = predict(logreg_521, type = "response")
rawresids = total_521$AdjScoreFlag - predprobs


threshold = 0.5
table(total_521$AdjScoreFlag, logreg_521$fitted > threshold)


library("pROC")
roc(total_521$AdjScoreFlag, fitted(logreg_521), plot=T, legacy.axes=T) # 0.5479

# Scorecard (multinomial logistic)
# 1: A = Top 25%, 2: B = 25-50%, 3: C = 50-75%, 4: F = < 75%
# quantile in R uses median
total_521 <- total_five_star[which(total_five_star$Measure.Code == '521'),]
total_521 <- na.omit(total_521)

total_521$AdjScoreGrade <- 0

total_521$AdjScoreGrade[total_521$Adjusted.Score >= (quantile(total_521$Adjusted.Score, .75))] <- 1
total_521$AdjScoreGrade[total_521$Adjusted.Score >= (quantile(total_521$Adjusted.Score, .50)) & total_521$Adjusted.Score < (quantile(total_521$Adjusted.Score, .75)) ] <- 2
total_521$AdjScoreGrade[total_521$Adjusted.Score >= (quantile(total_521$Adjusted.Score, .25)) & total_521$Adjusted.Score < (quantile(total_521$Adjusted.Score, .50)) ] <- 3
total_521$AdjScoreGrade[total_521$Adjusted.Score < (quantile(total_521$Adjusted.Score, .25))] <- 4

library(nnet)
multinomlogreg_521 = multinom(AdjScoreGrade ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_521)

table(total_521$AdjScoreGrade)

predprobs = fitted(multinomlogreg_521)

rawresid1 = (total_521$AdjScoreGrade == 1) -  predprobs[,1]

multiclass.roc(total_521$AdjScoreGrade, fitted(multinomlogreg_521), plot=T, legacy.axes=T) 
