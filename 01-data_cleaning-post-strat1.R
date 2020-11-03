#### Preamble ####
# Purpose: Clean and organize 2020 Election survey data for post stratify analysis
# Author: Adel Boufama
# Data: 29 October 2020
# Contact: adel.boufama@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/adel386/STA304_A3")
raw_data <- read_dta("usa_00002.dta")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
names(raw_data)

reduced_data <- 
  raw_data %>% 
  select(region,
         #stateicp,
         sex, 
         age, 
         race, 
         hispan,
         #marst, 
         #bpl,
         #citizen,
         #educd,
         #labforce,
         hhincome
         #,density
         )
rm(raw_data)         


#### What's next? ####
head(reduced_data)
#reduced_data$labforce %>% table()

reduced_data<-
  reduced_data %>%
  mutate(age_group = case_when(
    18 <= as.numeric(age) & as.numeric(age) <= 24 ~ "18-24",
    25 <= as.numeric(age) & as.numeric(age) <= 34 ~ "25-34",
    35 <= as.numeric(age) & as.numeric(age) <= 49 ~ "35-49",
    50 <= as.numeric(age) & as.numeric(age) <= 64 ~ "50-64",
    as.numeric(age) >= 65 ~ "65<"
  )
  
  )

reduced_data<-
  reduced_data %>%
  mutate(hhincome_bracket = case_when(
    hhincome <= 24999 ~ "under 25k",
    25000 <= hhincome & hhincome <= 49999 ~ "25k-50k",
    50000 <= hhincome & hhincome <= 99999 ~ "50k-100k",
    100000 <= hhincome & hhincome <= 199999 ~ "100k-200k",
    hhincome >= 200000 ~ "above 200k",
  
  )
  
  )


reduced_data<-
  reduced_data %>%
  mutate(race_groups = case_when(
    race == "white" & hispan =="not hispanic" ~ "white",
    race == "black/african american/negro" & hispan =="not hispanic" ~ "black",
    race == "chinese" | race == "japanese" & hispan =="not hispanic"  ~ "east_asian",
    race == "american indian or alaska native" & hispan =="not hispanic" ~ "native_american",
    race == "other asian or pacific islander" & hispan =="not hispanic" ~ "other_asian",
    race == "other race, nec" | race == "two major races" | race == "three or more major races" & hispan =="not hispanic" ~ "other",
    hispan == "mexican" | hispan == "puerto rican" | hispan == "cuban" | hispan == "other" | hispan == "not reported" ~ "hispanic"
    
  )
  
  )


reduced_data<-
  reduced_data %>%
  mutate(region_groups = case_when(
    region == "new england division" | region == "middle atlantic division" ~ "Northeast",
    region == "east north central div" | region == "west north central div" ~ "Midwest",
    region == "south atlantic division" | region == "east south central div" | region == "west south central div" ~ "South",
    region == "mountain division" | region == "pacific division" ~ "West"
   
  )
  
  )


## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

reduced_data <- 
  reduced_data %>%
  count(age_group, sex, race_groups, region_groups, hhincome_bracket) %>%
  group_by(age_group, sex, race_groups, region_groups, hhincome_bracket) 

reduced_data <- na.omit(reduced_data)
# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "census_data.csv")



         