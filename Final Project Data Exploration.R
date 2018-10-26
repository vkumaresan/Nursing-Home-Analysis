# Modeling Project
# Nursing Home Medicare Quality Measure Analysis
# Viggy Kumaresan

# Import datasets
medicare_quality <- read.csv('Medicare_Claims_Quality_Measures.csv')
head(medicare_quality)

provider <- read.csv('Provider_Info.csv')
head(provider)

# Look at quality measured provided in our dataset

table(medicare_quality$Measure.Description)

# 4 outcomes
# Number of hospitalizations per 1000 long-stay resident days 

# Percentage of short-stay residents who had an outpatient emergency department visit

# Percentage of short-stay residents who were rehospitalized after a nursing home admission

# Percentage of short-stay residents who were successfully discharged to the community


# Merge datasets on Federal Provider number

total <- merge(medicare_quality, provider, by = 'Federal.Provider.Number')
# maintained all records



