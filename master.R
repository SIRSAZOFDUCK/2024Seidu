## 

## Set up

rm(list = ls())

setwd("C:/Users/sirsa/OneDrive/Documents/2024Seidu")

library(dplyr)
library(fingertipsR)


## Load Fingertips data -------------

# Fingertips API (see https://fingertips.phe.org.uk/documents/fingertips_api_guide.pdf and https://github.com/ropensci/fingertipsR)
# profiles <- profiles() # Show PHOF profiles. Use to identify which profile set you need. Here we use profile ID 26 for local authorities
# inds <- indicators(ProfileID = "26") # Show indicators for selected PHOF profile. Here we use indicator ID 93553 [IMD 2019 score]
# areas <- area_types() # Show area types. Here we use area type 402 [Upper tier local authority] n=126

# Life expectancy at birth

  data <- fingertips_data(IndicatorID = 90366, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    # Keep required fields only
    select(AreaName, IndicatorName, Sex, Value) 

  # Males

  data1 <- data %>%
    # Keep one sex
    filter(Sex == "Male") %>%
    # Rename value to specific indicator
    rename("le.birth.m" = "Value") %>%
    # Keep required columns
    select(-c(IndicatorName, Sex))
  
  # Females
  
  data2 <- data %>%
    filter(Sex == "Female") %>%
    rename("le.birth.f" = "Value") %>%
    select(-c(IndicatorName, Sex))

# IMD (2019) scores

  data3 <- fingertips_data(IndicatorID = 93553, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2019") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("imd" = "Value")
  
# % people in employment

  data4 <- fingertips_data(IndicatorID = 92313, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022/23") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    filter(Age == "16-64 yrs") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("employment" = "Value")

  
# Homelessness: households owed a duty under the Homelessness Act
  
  data5 <- fingertips_data(IndicatorID = 93736, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022/23") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("homelessness" = "Value")
  

# Smoking prevalence
  
  data6 <- fingertips_data(IndicatorID = 92443, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("smoking" = "Value")
  
 
# Overweight (including obesity) prevalence in adults
  
  data7 <- fingertips_data(IndicatorID = 93088, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022/23") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("overweight" = "Value")
  
# New STI diagnoses (excluding chlamydia aged under 25) per 100,000
  
  data8 <- fingertips_data(IndicatorID = 91306, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("chlamydia" = "Value")
  
# Percentage of physically active adults

  data9 <- fingertips_data(IndicatorID = 93014, AreaTypeID = 502) %>% #
    # Keep required time period
    filter(Timeperiod == "2022/23") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("pa" = "Value")

# Average Attainment 8 score 
  
  data10 <- fingertips_data(IndicatorID = 93378, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022/23") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("attainment8" = "Value") 

# Emergency Hospital Admissions for Intentional Self-Harm

  data11 <- fingertips_data(IndicatorID = 21001, AreaTypeID = 502) %>% #
    # Keep required time period
    filter(Timeperiod == "2022/23") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("adm.selfharm" = "Value") 


# Under 75 mortality rate from cancer  
  
  data12 <- fingertips_data(IndicatorID = 40501, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("mort.cancer" = "Value")

# Supporting information - % population aged 65+
  
  data13 <- fingertips_data(IndicatorID = 92310, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("pop.over65" = "Value")
  
 #  Supporting information - % population aged under 18
  
  data14 <- fingertips_data(IndicatorID = 92309, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("pop.under18" = "Value")
  
 # Suicide rate
  
  data15 <- fingertips_data(IndicatorID = 41001, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2020 - 22") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("suicide" = "Value")

# Violent crime - hospital admissions for violence (including sexual violence)
  
  data16 <- fingertips_data(IndicatorID = 11201, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2020/21 - 22/23") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("adm.violent.crime" = "Value")
  
# Infant mortality rate  
  
  data17 <- fingertips_data(IndicatorID = 92196, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2020 - 22") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("infant.mort" = "Value")
  
# Children in absolute low income families (under 16s)

  data18 <- fingertips_data(IndicatorID = 93701, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022/23") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("children.low.income" = "Value")
 
# Under 18s conception rate / 1,000

  data19 <- fingertips_data(IndicatorID = 20401, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2021") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("teen.pregnancy" = "Value")

# Under 75 mortality rate from all circulatory diseases
  
  data20 <- fingertips_data(IndicatorID = 40401, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("mort.cvd" = "Value")

# Average weekly earnings
  
  data21 <- fingertips_data(IndicatorID = 93351, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("weekly.earnings" = "Value")
  
# Fuel poverty (low income, low energy efficiency methodology)
  
  data22 <- fingertips_data(IndicatorID = 93759, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2021") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("fuel.povery" = "Value")
  
# New STI diagnoses (excluding chlamydia aged under 25) per 100,000
  
  data23 <- fingertips_data(IndicatorID = 91306, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("sti.rates" = "Value")
  
 # People receiving an NHS Health Check per year
  
  data24 <- fingertips_data(IndicatorID = 91734, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022/23") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("health.check" = "Value")
  

# Diabetes: QOF prevalence (17+ yrs)
  
  data25 <- fingertips_data(IndicatorID = 241, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022/23") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("diabetes" = "Value")
  
# Hypertension: QOF prevalence (all ages)  
  
  data26 <- fingertips_data(IndicatorID = 219, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022/23") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("htn" = "Value")

# CHD: QOF prevalence (all ages)
  
  data27 <- fingertips_data(IndicatorID = 273, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022/23") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("chd" = "Value")

# Under 75 mortality rate from cardiovascular diseases considered preventable (2016 definition) - REMOVE? REPEAT OF ABOVE?
  
  data28 <- fingertips_data(IndicatorID = 40402, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022/23") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    filter(Sex == "Persons") %>%
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("mort.chd" = "Value") 
 
# Cancer screening coverage: breast cancer
  
  data29 <- fingertips_data(IndicatorID = 22001, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("screening.breast" = "Value") 
  
  
# Cancer screening coverage: bowel cancer
  
  data30 <- fingertips_data(IndicatorID = 91720, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("screening.bowel" = "Value") 
  
# Cancer screening coverage: cervical cancer (aged 25 to 49 years old)

  data31 <- fingertips_data(IndicatorID = 93560, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("screening.cervical.younger" = "Value") 
  
  
# Cancer screening coverage: cervical cancer (aged 50 to 64 years old)
  
  data32 <- fingertips_data(IndicatorID = 93561, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("screening.cervical.older" = "Value") 
  
  
 # Emergency admissions (under 18 years)
  
    data33 <- fingertips_data(IndicatorID = 92702, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2022/23") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      filter(Sex == "Persons") %>%
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("adm.emergency.under18" = "Value") 
  
# Under 75 mortality rate from causes considered preventable    
    
    data34 <- fingertips_data(IndicatorID = 93721, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2022") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      filter(Sex == "Persons") %>%
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("mort.preventable" = "Value") 
    
#  Air pollution: fine particulate matter (new method - concentrations of total PM2.5)
    
    data35 <- fingertips_data(IndicatorID = 93867, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2022") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("pollution.pm2.5" = "Value") 

# Fraction of mortality attributable to particulate air pollution (new method)

    data36 <- fingertips_data(IndicatorID = 93861, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2022") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("mort.pollution" = "Value") 
    
# Households with overcrowding based on overall room occupancy levels
    
    data37 <- fingertips_data(IndicatorID = 93277, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2022") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("overcrowding" = "Value") 

# Depression: QOF prevalence (18+ yrs)
    
    data38 <- fingertips_data(IndicatorID = 848, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2022/23") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("depression" = "Value")     
  
# Social Isolation: percentage of adult social care users who have as much social contact as they would like 18+ years
    
    data39 <- fingertips_data(IndicatorID = 90280, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2022/23") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      filter(Age == "18+ yrs") %>%
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("social.contact" = "Value")     
    
# Violent crime - violence offences per 1,000 population
    
    data40 <- fingertips_data(IndicatorID = 11202, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2022/23") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("violent.crime" = "Value")    

# Low birth weight of term babies
    
    data41 <- fingertips_data(IndicatorID = 20101, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2021") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("lbw" = "Value")    
    
# Breastfeeding prevalence at 6 to 8 weeks - current method  
    
    data42 <- fingertips_data(IndicatorID = 92517, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2022/23") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("lbw" = "Value")   
    
    
    

#  inds <- indicators()
# LE at age 75, M and F

# Fuel poverty (latest is 2021)
# Alcohol consumption (can't find an indicator with recent data)
# GP patient experience scores (not at LA level)
# Cancer incidence rates (have cancer mortality)
# Household overcrowing (n recent data)
# Population density (no recent data)
# Ethnicity (latest I can find is 2016)
# Self-reported wellbeing (last data 2020/21)
# Work-related illness rates (;ast data 20/21)
# Teenage preganncy rate (latest I can find is 2021)