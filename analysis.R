# Workbook 6: analyze NHANES data

# Set up
library(survey)
library(Hmisc)

demo <- sasxport.get("DEMO_I.XPT")
alc <- sasxport.get("ALQ_I.XPT")

nhanes <- merge(x = demo, y = alc, by = "seqn", all = TRUE)

wt_sum <- sum(nhanes$wtint2yr, na.rm = TRUE)
# This represents overall US population 

## Analysis 
# ALQ151, 2 = 0, ignore 7, 9
nhanes$alq151[nhanes$alq151 == 2] <- 0
nhanes$alq151[nhanes$alq151 == 7] <- NA
nhanes$alq151[nhanes$alq151 == 9] <- NA

# create survey design 
nhanes_survey <- svydesign(
  id = ~sdmvpsu,
  nest = TRUE,
  strata = ~sdmvstra,
  weights = ~wtint2yr,
  data = nhanes
)

nhanes_mean <- svymean(~alq151, nhanes_survey, na.rm = TRUE)
nhanes_mean_gender <- svyby(~alq151, ~riagendr, nhanes_survey, svymean, na.rm = TRUE)


