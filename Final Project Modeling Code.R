# Final Project Modeling Code

### 430: Percentage of short-stay residents assessed and appropriately given the pneumococcal vaccine 
total_430 <- total[which(total$Measure.Code == '430'),]

# Plot distribution of outcome variable
hist(total_430$Four.Quarter.Average.Score)

# Test linear regression models for each outcome
reg_430 = lm(Four.Quarter.Average.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds^2 + Number.of.Certified.Beds , data= total_430)
summary(reg_430)

reg_430_min = lm(Four.Quarter.Average.Score ~ Total.Weighted.Health.Survey.Score + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_430)
summary(reg_430)

# Test quarters
reg_430_1 = lm(Q1.Measure.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_430)
summary(reg_430_1)

reg_430_2 = lm(Q2.Measure.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_430)
summary(reg_430_2)

reg_430_3 = lm(Q3.Measure.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_430)
summary(reg_430_3)

reg_430_4 = lm(Q4.Measure.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_430)
summary(reg_430_4)

# Small differences among quarter predictions

# Log Transformed

table(total_430$Four.Quarter.Average.Score) # only 5 providers have 'zero' values, so we can try to remove them and log-transform values
total_430 <- total_430[which(total_430$Four.Quarter.Average.Score != 0),]
reg_430_log = lm(log(Four.Quarter.Average.Score) ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_430)
summary(reg_430_log)

# Plot predictor variables vs outcome variable
plot(Four.Quarter.Average.Score ~ Total.Weighted.Health.Survey.Score +  Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_430, ask=T)

plot(log(Four.Quarter.Average.Score^2) ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_430, ask=T)
hist(log(total_430$Four.Quarter.Average.Score))

# Try Box-Cox transformation 
library(MASS)
box_cox_430 <- boxcox(Four.Quarter.Average.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_430)
trans <- box_cox_430$x[which.max(box_cox_430$y)]

reg_430_bc = lm(((Four.Quarter.Average.Score^trans-1)/trans) ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_430)
summary(reg_430_bc) # 0.104

# Convert outcome to binary outcome
# 1: Greater than pop average, 0 otherwise
total_430 <- total[which(total$Measure.Code == '430'),]
total_430 <- na.omit(total_430)

mean(total_430$Four.Quarter.Average.Score)
total_430$AdjScoreFlag <- 0

total_430 <- na.omit(total_430)
total_430$AdjScoreFlag[total_430$Four.Quarter.Average.Score > (mean(total_430$Four.Quarter.Average.Score))] <- 1
logreg_430 = glm(AdjScoreFlag ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, family = binomial, data= total_430)
summary(logreg_430)

predprobs = predict(logreg_430, type = "response")
rawresids = total_430$AdjScoreFlag - predprobs


threshold = 0.5
table(total_430$AdjScoreFlag, logreg_430$fitted > threshold)


library("pROC")
roc(total_430$AdjScoreFlag, fitted(logreg_430), plot=T, legacy.axes=T) # 0.6731

# Scorecard (multinomial logistic)
# 1: A = Top 25%, 2: B = 25-50%, 3: C = 50-75%, 4: F = < 75%
# quantile in R uses median
total_430 <- total[which(total$Measure.Code == '430'),]
total_430 <- na.omit(total_430)
total_430$AdjScoreGrade <- 0

total_430$AdjScoreGrade[total_430$Four.Quarter.Average.Score >= (quantile(total_430$Four.Quarter.Average.Score, .75))] <- 1
total_430$AdjScoreGrade[total_430$Four.Quarter.Average.Score >= (quantile(total_430$Four.Quarter.Average.Score, .50)) & total_430$Four.Quarter.Average.Score < (quantile(total_430$Four.Quarter.Average.Score, .75)) ] <- 2
total_430$AdjScoreGrade[total_430$Four.Quarter.Average.Score >= (quantile(total_430$Four.Quarter.Average.Score, .25)) & total_430$Four.Quarter.Average.Score < (quantile(total_430$Four.Quarter.Average.Score, .50)) ] <- 3
total_430$AdjScoreGrade[total_430$Four.Quarter.Average.Score < (quantile(total_430$Four.Quarter.Average.Score, .25))] <- 4

library(nnet)
multinomlogreg_430 = multinom(AdjScoreGrade ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_430)

table(total_430$AdjScoreGrade)

predprobs = fitted(multinomlogreg_430)

rawresid1 = (total_430$AdjScoreGrade == 1) -  predprobs[,1]

multiclass.roc(total_430$AdjScoreGrade, fitted(multinomlogreg_430), plot=T, legacy.axes=T) 











### 410: Percentage of long-stay residents experiencing one or more falls with major injury 
# Long Stay
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












# 551 (Five-Star Rating)
total_551 <- total_five_star[which(total_five_star$Measure.Code == '551'),]

names(total_five_star)
reg_551 = lm(log(Adjusted.Score^2) ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_551)
summary(reg_551)

hist(total_551$Adjusted.Score)
plot(Adjusted.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_551, ask=T)


table(total_551$Adjusted.Score) # only 19 providers have 'zero' values, so we can try to remove them and log-transform values
total_551 <- total_551[which(total_551$Adjusted.Score != 0),]
reg_551 = lm(log(Adjusted.Score) ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_551)
summary(reg_551)


# Poisson

poisreg_551 = glm(Adjusted.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, family = poisson, data= total_551)
summary(poisreg_551)

predprobs = predict(poisreg_551, type = "response")
rawresids = total_551$Adjusted.Score - predprobs

anova(poisreg_551, rawresids, test = "Chisq")

# Convert outcome to binary outcome
# 1: Greater than pop average, 0 otherwise
total_551 <- total_five_star[which(total_five_star$Measure.Code == '551'),]

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

cor(total_551$Total.Weighted.Health.Survey.Score, total_551$Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection)
cor(total_551$Number.of.Health.Deficiencies.on.Survey.Under.New.Process, total_551$Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection)
cor(total_551$Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day, total_551$Adjusted.CNA.Staffing.Hours.per.Resident.per.Day)
cor(total_551$Number.of.Certified.Beds, total_551$Adjusted.RN.Staffing.Hours.per.Resident.per.Day)
