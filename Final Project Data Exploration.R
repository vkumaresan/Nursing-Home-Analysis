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

names(total)



## Visualizations

library(tidyverse)

# Average measure by State

### Long Stay
# 410
total_410 <- total[which(total$Measure.Code == '410'),]
total_410 <- na.omit(total_410)

ggplot(total_410, aes(x=reorder(Provider.State.x, -Four.Quarter.Average.Score), y = Four.Quarter.Average.Score)) + 
  stat_summary(fun.y="mean", geom="bar", fill = "steelblue") + 
  xlab("Provider State") +
  ylab("Average Measure Score") + 
  ggtitle('Average percentage of long-stay residents experiencing one or more falls with major injury (risk-adjusted)') +
  theme_minimal() 

# 402
total_402 <- total[which(total$Measure.Code == '402'),]
total_402 <- na.omit(total_402)

ggplot(total_402, aes(x=reorder(Provider.State.x, -Four.Quarter.Average.Score), y = Four.Quarter.Average.Score)) + 
  stat_summary(fun.y="mean", geom="bar", fill = "steelblue") + 
  xlab("Provider State") +
  ylab("Average Measure Score") + 
  ggtitle('Average Percentage of long-stay residents who self-report moderate to severe pain') +
  theme_minimal() 

# 551
total_551 <- total_five_star[which(total_five_star$Measure.Code == '551'),]
total_551 <- na.omit(total_551)

ggplot(total_551, aes(x=reorder(Provider.State.x, -Adjusted.Score), y = Adjusted.Score)) + 
  stat_summary(fun.y="mean", geom="bar", fill = "steelblue") + 
  xlab("Provider State") +
  ylab("Average Measure Score") + 
  ggtitle('Average Number of hospitalizations per 1000 long-stay resident days') +
  theme_minimal()



### Short Stay
# 424
total_424 <- total[which(total$Measure.Code == '425'),]
total_424 <- na.omit(total_425)

ggplot(total_424, aes(x=reorder(Provider.State.x, -Four.Quarter.Average.Score), y = Four.Quarter.Average.Score)) + 
  stat_summary(fun.y="mean", geom="bar", fill = "steelblue") + 
  xlab("Provider State") +
  ylab("Average Measure Score") + 
  ggtitle('Average Percentage of short-stay residents who self-report moderate to severe pain') +
  theme_minimal() 

# 523
total_523 <- total_five_star[which(total_five_star$Measure.Code == '523'),]
total_523 <- na.omit(total_523)

ggplot(total_523, aes(x=reorder(Provider.State.x, -Adjusted.Score), y = Adjusted.Score)) + 
  stat_summary(fun.y="mean", geom="bar", fill = "steelblue") + 
  xlab("Provider State") +
  ylab("Average Measure Score") + 
  ggtitle('Average percentage of short-stay residents who were successfully discharged to the community') +
  theme_minimal() 

#521
total_521 <- total_five_star[which(total_five_star$Measure.Code == '521'),]
total_521<- na.omit(total_521)

ggplot(total_521, aes(x=reorder(Provider.State.x, -Adjusted.Score), y = Adjusted.Score)) + 
  stat_summary(fun.y="mean", geom="bar", fill = "steelblue") + 
  xlab("Provider State") +
  ylab("Average Measure Score") + 
  ggtitle('Average percentage of short-stay residents who were rehospitalized after a nursing home admission (risk-adjusted)') +
  theme_minimal() 


# Model Outputs

linear_reg <- data.frame("Measure Code" = c(410, 402, 551, 424, 523, 521), "Measure Description" = c('Percentage of long-stay residents experiencing one or more falls with major injury', 'Percentage of long-stay residents who self-report moderate to severe pain', 'Number of hospitalizations per 1000 long-stay resident days', 'Percentage of short-stay residents who self-report moderate to severe pain', 'Percentage of short-stay residents who were successfully discharged to the community', 'Percentage of short-stay residents who were rehospitalized after a nursing home admission'), "R-Squared" = c(0.02505, 0.06627, 0.07023, 0.05545, 0.05102, 0.01249), "Type" = c("Long Stay", "Long Stay", "Long Stay", "Short Stay", "Short Stay", "Short Stay"))


ggplot(linear_reg, aes(x = reorder(as.factor(Measure.Description), R.Squared), y = R.Squared, fill = Type), color = Type) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  xlab("Measure Description") + 
  ylab("R-Squared") + 
  ggtitle("Linear Regression Performance across Measures") + 
  theme(axis.text.x = element_text(face="bold", color="#993333",angle=45)) +
  theme_minimal() + 
  coord_flip()


logistic_reg <- data.frame("Measure Code" = c(410, 402, 551, 424, 523, 521), "Measure Description" = c('Percentage of long-stay residents experiencing one or more falls with major injury', 'Percentage of long-stay residents who self-report moderate to severe pain', 'Number of hospitalizations per 1000 long-stay resident days', 'Percentage of short-stay residents who self-report moderate to severe pain', 'Percentage of short-stay residents who were successfully discharged to the community', 'Percentage of short-stay residents who were rehospitalized after a nursing home admission'), "AUC" = c(0.5932, 0.615, 0.6271, 0.6058, 0.6424, 0.5479), "Type" = c("Long Stay", "Long Stay", "Long Stay", "Short Stay", "Short Stay", "Short Stay"))

ggplot(logistic_reg, aes(x = reorder(as.factor(Measure.Description), AUC), y = AUC, fill = Type), color = Type) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  xlab("Measure Description") + 
  ylab("AUC") + 
  ggtitle("Logistic Regression Performance across Measures") + 
  theme(axis.text.x = element_text(face="bold", color="#993333",angle=45)) +
  theme_minimal() +
  coord_flip()

       