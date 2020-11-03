#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/adel386/STA304_A3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data_survey <- read_dta("ns20200625/ns20200625.dta")
# Add the labels
raw_data_survey <- labelled::to_factor(raw_data_survey)
# Just keep some variables
reduced_data_survey <- 
  raw_data_survey %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         #employment,
         #foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         #education,
         state,
         #congress_district,
         age)


#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?



reduced_data_survey<-
  reduced_data_survey %>%
  mutate(vote_trump = 
           ifelse(vote_2020 == "Donald Trump", 1, 0))

reduced_data_survey<-
  reduced_data_survey %>%
  mutate(vote_biden = 
           ifelse(vote_2020 == "Joe Biden", 1, 0))




reduced_data_survey<-
  reduced_data_survey %>%
  mutate(age_group = case_when(
    18 <= age & age <= 24 ~ "18-24",
    25 <= age & age <= 34 ~ "25-34",
    35 <= age & age <= 49 ~ "35-49",
    50 <= age & age <= 64 ~ "50-64",
    age >= 65 ~ "65<"
  )
  
  )

reduced_data_survey<-
  reduced_data_survey %>%
  mutate(sex = case_when(
    gender == "Female" ~ "female",
    gender == "Male" ~ "male"
  )
  
  )

reduced_data_survey<-
  reduced_data_survey %>%
  mutate(region_groups = case_when(
    census_region == "Midwest" ~ "Midwest",
    census_region == "West" ~ "West",
    census_region == "Northeast" ~ "Northeast",
    census_region == "South" ~ "South"
  )
  
  )

reduced_data_survey<-
  reduced_data_survey %>%
  mutate(hhincome_bracket = case_when(
    household_income == "Less than $14,999" | household_income == "$15,000 to $19,999" | household_income == "$20,000 to $24,999" ~ "under 25k",
    household_income == "$25,000 to $29,999" | household_income == "$30,000 to $34,999" | household_income == "$35,000 to $39,999" | household_income == "$40,000 to $44,999" | household_income == "$45,000 to $49,999" ~ "25k-50k",
    household_income == "$50,000 to $54,999" | household_income == "$55,000 to $59,999" | household_income == "$60,000 to $64,999" | household_income == "$65,000 to $69,999" | household_income == "$70,000 to $74,999" | household_income == "$75,000 to $79,999" | household_income == "$80,000 to $84,999" | household_income == "$85,000 to $89,999" | household_income == "$90,000 to $94,999" | household_income == "$95,000 to $99,999" ~ "50k-100k",
    household_income == "$100,000 to $124,999" | household_income == "$125,000 to $149,999" | household_income == "$150,000 to $174,999" | household_income == "$175,000 to $199,999" ~ "100k-200k",
    household_income == "$200,000 to $249,999" | household_income == "$250,000 and above" ~ "above 200k"
   
  )
  
  )


reduced_data_survey<-
  reduced_data_survey %>%
  mutate(race_groups = case_when(
    race_ethnicity == "White" & hispanic == "Not Hispanic" ~ "white",
    race_ethnicity == "Black, or African American" & hispanic == "Not Hispanic" ~ "black",
    race_ethnicity == "Asian (Chinese)" & hispanic == "Not Hispanic" | race_ethnicity == "Asian (Filipino)" | race_ethnicity == "Asian (Japanese)"  | race_ethnicity == "Asian (Vietnamese)" | race_ethnicity == "Asian (Korean)" ~ "east_asian",
    race_ethnicity == "American Indian or Alaska Native" & hispanic == "Not Hispanic"  ~ "native_american",
    race_ethnicity == "Pacific Islander (Native Hawaiian)" & hispanic == "Not Hispanic" | race_ethnicity == "Asian (Other)" | race_ethnicity == "Asian (Asian Indian)" | race_ethnicity == "Pacific Islander (Guamanian)" | race_ethnicity == "Pacific Islander (Other)" | race_ethnicity == "Pacific Islander (Samoan)" ~ "other_asian",
    race_ethnicity == "Some other race" & hispanic == "Not Hispanic" ~ "other",
    hispanic == "Mexican" | hispanic == "Puerto Rican" | hispanic == "Cuban" | hispanic == "Argentinian" | hispanic == "Colombian" | hispanic == "Ecuadorian" | hispanic == "Salvadorean" | hispanic == "Guatemalan" | hispanic == "Nicaraguan" | hispanic == "Panamanian" | hispanic == "Peruvian" | hispanic == "Spanish" | hispanic == "Venezuelan" | hispanic == "Other Hispanic" ~ "hispanic"
    
  )
  
  )


# Saving the survey/sample data as a csv file in my
# working directory
reduced_data_survey <- reduced_data_survey %>%
  select(interest,
        registration,
        vote_2016,
        vote_intention,
        vote_2020,
        ideo5,
        age_group,
        sex,
        race_groups,
        region_groups,
        hhincome_bracket,
        state,
        vote_trump,
        vote_biden
         )

# get rid of all rows with any NA data
reduced_data_survey <- na.omit(reduced_data_survey)

write_csv(reduced_data_survey, "survey_data.csv")

