# Modeling Project
# Nursing Home Medicare Quality Measure Analysis
# Viggy Kumaresan

# Import datasets

# Outcome data: Long and short stay quality measures from MDS
quality <- read.csv('MDS_Quality_Measures.csv')
head(quality)

# New additional measures for Medicare Five-Star Rating system (additional potential outcome data)
five_star <- read.csv('Medicare_Claims_Quality_Measures.csv')
head(five_star)

# Predictor data: Nursing Home information
provider <- read.csv('Provider_Info.csv')
head(provider)


# Merge datasets on Federal Provider number

# merge outcome and predictor data into one total dataframe
total <- merge(quality, provider, by = 'Federal.Provider.Number')
length(unique(total$Federal.Provider.Number)) #15,613
colnames(total)
summary(total)

# merge five-star data and predictor data into one total dataframe
total_five_star <- merge(five_star, provider, by = 'Federal.Provider.Number')
length(unique(total_five_star$Federal.Provider.Number)) #15,613
colnames(total_five_star)
summary(total_five_star)


# Look at quality measures provided in our datasets
table(total$Measure.Description) # MDS
table(total_five_star$Measure.Description) # Five Star

## Provider dataset exploration
colnames(provider)
str(provider)

# Factor variables
table(provider$Ownership.Type)
table(provider$Provider.Type)
table(provider$Continuing.Care.Retirement.Community)
table(provider$With.a.Resident.and.Family.Council)
table(provider$Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process)
table(provider$Largest.Number.of.Residents.Affected.by.Deficiency.Cited.Under.New.Process)

# Adjust types of other predictor variables
str(total)
total$Number.of.Health.Deficiencies.on.Survey.Under.New.Process <- as.numeric(total$Number.of.Health.Deficiencies.on.Survey.Under.New.Process)
total_five_star$Number.of.Health.Deficiencies.on.Survey.Under.New.Process <- as.numeric(total_five_star$Number.of.Health.Deficiencies.on.Survey.Under.New.Process)


# Primary flag used to differentiate long and short term stay measures: Resident Type
table(total$Resident.type)

# Create sub-data frames for each outcome measure
# Short Stay measures
total_430 <- total[which(total$Measure.Code == '430'),]
total_471 <- total[which(total$Measure.Code == '471'),]
total_434 <- total[which(total$Measure.Code == '434'),]
total_424 <- total[which(total$Measure.Code == '424'),]
total_426 <- total[which(total$Measure.Code == '426'),]
total_425 <- total[which(total$Measure.Code == '426'),]
total_521 <- total_five_star[which(total_five_star$Measure.Code == '521'),]
total_522 <- total_five_star[which(total_five_star$Measure.Code == '522'),]
total_523 <- total_five_star[which(total_five_star$Measure.Code == '523'),]

# Long Stay measures
total_403 <- total[which(total$Measure.Code == '403'),]
total_415 <- total[which(total$Measure.Code == '415'),]
total_411 <- total[which(total$Measure.Code == '411'),]
total_410 <- total[which(total$Measure.Code == '410'),]
total_408 <- total[which(total$Measure.Code == '408'),]
total_404 <- total[which(total$Measure.Code == '404'),]
total_452 <- total[which(total$Measure.Code == '452'),]
total_419 <- total[which(total$Measure.Code == '419'),]
total_402 <- total[which(total$Measure.Code == '402'),]
total_409 <- total[which(total$Measure.Code == '409'),]
total_451 <- total[which(total$Measure.Code == '451'),]
total_401 <- total[which(total$Measure.Code == '401'),]
total_406 <- total[which(total$Measure.Code == '406'),]
total_407 <- total[which(total$Measure.Code == '407'),]
total_405 <- total[which(total$Measure.Code == '405'),]
total_551 <- total_five_star[which(total_five_star$Measure.Code == '551'),]



# Test linear regression models for each outcome
reg_430 = lm(Four.Quarter.Average.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_430)
summary(reg_430)

# Plot distribution of outcome variable
hist(total_430$Four.Quarter.Average.Score)

# Plot predictor variables vs outcome variable
plot(Four.Quarter.Average.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_430, ask=T)



names(total_five_star)
reg_551 = lm(Adjusted.Score ~ Total.Weighted.Health.Survey.Score + Number.of.Health.Deficiencies.on.Previous.Standard.Health.Inspection + Severity.of.Most.Severe.Deficiency.Cited.Under.New.Process + Number.of.Health.Deficiencies.on.Survey.Under.New.Process + Adjusted.Total.Nurse.Staffing.Hours.per.Resident.per.Day + Adjusted.RN.Staffing.Hours.per.Resident.per.Day + Adjusted.LPN.Staffing.Hours.per.Resident.per.Day + Adjusted.CNA.Staffing.Hours.per.Resident.per.Day + Health.Inspection.Rating + Automatic.Sprinkler.Systems.in.All.Required.Areas + With.a.Resident.and.Family.Council + Provider.Changed.Ownership.in.Last.12.Months + Number.of.Certified.Beds, data= total_551)
summary(reg_551)

hist(total_551$Adjusted.Score)
