## Predicting inequalities in life expectancy by local authority using routine public health data
## Seidu et al. 2024



## Set up -----------------

# Clear environment
rm(list = ls())

# Set working directory
setwd("C:/Users/sirsa/OneDrive/Documents/2024Seidu") # Change to your own folder

# Install packages
list.of.packages <- c("dplyr", "fingertipsR", "purrr", "VIM", "caret", "corrplot", "glmnet", "randomForest", "ggplot2", "MASS", "Rtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

options(repos = c(
  ropensci = 'https://ropensci.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

install.packages("fingertipsR")

# Load packages
library(dplyr)
library(fingertipsR)
library(purrr)
library(VIM)
library(caret)
library(corrplot)
library(glmnet)
library(randomForest)
library(ggplot2)
library(MASS)



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

# Cancer screening coverage: breast cancer
  data28 <- fingertips_data(IndicatorID = 22001, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("screening.breast" = "Value") 
  
  
# Cancer screening coverage: bowel cancer
  data29 <- fingertips_data(IndicatorID = 91720, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("screening.bowel" = "Value") 
  
# Cancer screening coverage: cervical cancer (aged 25 to 49 years old)
  data30 <- fingertips_data(IndicatorID = 93560, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("screening.cervical.younger" = "Value") 
  
  
# Cancer screening coverage: cervical cancer (aged 50 to 64 years old)
  data31 <- fingertips_data(IndicatorID = 93561, AreaTypeID = 502) %>% 
    # Keep required time period
    filter(Timeperiod == "2022") %>% 
    # Remove All England value
    filter(AreaType != "England") %>% 
    # Keep required fields only
    select(AreaName, Value) %>%
    # Rename value to specific indicator
    rename("screening.cervical.older" = "Value") 
  
  
 # Emergency admissions (under 18 years)
    data32 <- fingertips_data(IndicatorID = 92702, AreaTypeID = 502) %>% 
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
    data33 <- fingertips_data(IndicatorID = 93721, AreaTypeID = 502) %>% 
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
    data34 <- fingertips_data(IndicatorID = 93867, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2022") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("pollution.pm2.5" = "Value") 

# Fraction of mortality attributable to particulate air pollution (new method)
  data35 <- fingertips_data(IndicatorID = 93861, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2022") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("mort.pollution" = "Value") 
    
# Depression: QOF prevalence (18+ yrs)
    data36 <- fingertips_data(IndicatorID = 848, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2022/23") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("depression" = "Value")     
  
# Social Isolation: percentage of adult social care users who have as much social contact as they would like 18+ years
    data37 <- fingertips_data(IndicatorID = 90280, AreaTypeID = 502) %>% 
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
   data38 <- fingertips_data(IndicatorID = 11202, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2022/23") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("violent.crime" = "Value")    

# Low birth weight of term babies
   data39 <- fingertips_data(IndicatorID = 20101, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2021") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("lbw" = "Value")    
    
# Breastfeeding prevalence at 6 to 8 weeks - current method  
   data40 <- fingertips_data(IndicatorID = 92517, AreaTypeID = 502) %>% 
      # Keep required time period
      filter(Timeperiod == "2022/23") %>% 
      # Remove All England value
      filter(AreaType != "England") %>% 
      # Keep required fields only
      select(AreaName, Value) %>%
      # Rename value to specific indicator
      rename("lbw" = "Value")   
    
    
## Data processing -------------
    
    # Create a list of dataframe names from data1 to data42
    data_list <- sprintf("data%d", 1:40)
    
    
    # Merge datasets 
    all.data <- data1 %>%
      full_join(data2, by = "AreaName") %>%
      full_join(data3, by = "AreaName") %>%
      full_join(data4, by = "AreaName") %>%
      full_join(data5, by = "AreaName") %>%
      full_join(data6, by = "AreaName") %>%
      full_join(data7, by = "AreaName") %>%
      full_join(data8, by = "AreaName") %>%
      full_join(data9, by = "AreaName") %>%
      full_join(data10, by = "AreaName") %>%
      full_join(data11, by = "AreaName") %>%
      full_join(data12, by = "AreaName") %>%
      full_join(data13, by = "AreaName") %>%
      full_join(data14, by = "AreaName") %>%
      full_join(data15, by = "AreaName") %>%
      full_join(data16, by = "AreaName") %>%
      full_join(data17, by = "AreaName") %>%
      full_join(data18, by = "AreaName") %>%
      full_join(data19, by = "AreaName") %>%
      full_join(data20, by = "AreaName") %>%
      full_join(data21, by = "AreaName") %>%
      full_join(data22, by = "AreaName") %>%
      full_join(data23, by = "AreaName") %>%
      full_join(data24, by = "AreaName") %>%
      full_join(data25, by = "AreaName") %>%
      full_join(data26, by = "AreaName") %>%
      full_join(data27, by = "AreaName") %>%
      full_join(data28, by = "AreaName") %>%
      full_join(data29, by = "AreaName") %>%
      full_join(data30, by = "AreaName") %>%
      full_join(data31, by = "AreaName") %>%
      full_join(data32, by = "AreaName") %>%
      full_join(data33, by = "AreaName") %>%
      full_join(data34, by = "AreaName") %>%
      full_join(data35, by = "AreaName") %>%
      full_join(data36, by = "AreaName") %>%
      full_join(data37, by = "AreaName") %>%
      full_join(data38, by = "AreaName") %>%
      full_join(data39, by = "AreaName") %>%
      full_join(data40, by = "AreaName") 

    # Calculate inequality in life expectancy: female - male
    all.data <- all.data %>%
      mutate(le.inequality = le.birth.f - le.birth.m) %>%
      # Remove original life expectancy columns
      select(-c(le.birth.f, le.birth.m))

    # Impute missing values using the mean of the ten nearest neighbours
    
        # Decide which is the optimal number of nearest neighbours to use
        
        # Range of k values to test: 3, 5, 7, 10, 12
        
        # Get complete avaialble data
        data.original <- all.data %>%
          # Get complete rows only
          filter(complete.cases(.)) %>%
          select(-AreaName)
        
        # Create data with randomly inserted NA values where these are actually known
        data.missing <- all.data %>%
          # Get complete rows only
          filter(complete.cases(.)) %>%
          # Introduce some random NAs
          mutate(across(everything(), ~ ifelse(runif(n()) < 0.1, NA, .))) %>%
          select(-AreaName)
        
        # Calculate imputed data
        data.imputed <- kNN(data.missing, k = 12, imp_var = FALSE)
        
        # RMSE calculation function
        calculate_rmse <- function(true_values, imputed_values) {
          sqrt(mean((true_values - imputed_values)^2, na.rm = TRUE))
        }
        
        # MAE calculation function
        calculate_mae <- function(true_values, imputed_values) {
          mean(abs(true_values - imputed_values), na.rm = TRUE)
        }
        
        # Evaluate the imputation quality across the entire dataset
        evaluate_imputation <- function(original_data, imputed_data, missing_data) {
          rmse_values <- sapply(names(original_data), function(col) {
            calculate_rmse(original_data[[col]][is.na(missing_data[[col]])], 
                           imputed_data[[col]][is.na(missing_data[[col]])])
          })
          
          mae_values <- sapply(names(original_data), function(col) {
            calculate_mae(original_data[[col]][is.na(missing_data[[col]])], 
                          imputed_data[[col]][is.na(missing_data[[col]])])
          })
          
          list(RMSE = rmse_values, MAE = mae_values)
        }
        
        # Get RMSE and MAE for the entire dataset
        imputation_evaluation <- evaluate_imputation(data.original, data.imputed, data.missing)
        
        # Print sum of all RMSE and MAE values
        # Sum all RMSE values
        print(sum(imputation_evaluation$RMSE, na.rm = TRUE))
        print(sum(imputation_evaluation$MAE, na.rm = TRUE))
        
        # Choose the best k based on evaluation
        # k = 3: RMSE = 624, MAE = 411 
        # k = 5: RMSE = 553, MAE = 382
        # k = 7: RMSE = 607, MAE = 400
        # k = 10: RMSE = 620, MAE = 402
        # k = 12: RMSE = 626, MAE = 397
        # Optimal k = 5
        
        # Perform KNN imputation
        data_imputed <- kNN(all.data, k = 5, imp_var = FALSE) 

    # Select data for ML
    
        # Select relevant features for modeling
        selected_data <- data_imputed %>% select(-AreaName)
        
    
        
        
## Feature selection -----------
    
        
    # Standardise data
        
        # Standardise
        preProcValues <- preProcess(selected_data, method = c("center", "scale")) # subtract the mean and divide by SD, to scale centre zero, SD = 1
        data_standardized <- predict(preProcValues, selected_data)
        
    # Reduce multicollinearity
        
        # Create correlation matrix
        correlation_matrix <- cor(data_standardized)
        
        # Plot the correlation matrix
        corrplot(correlation_matrix, method = "circle")
        
        # Set a threshold for high correlation
        high_correlation_threshold <- 0.8
        
        # Find pairs of highly correlated features
        highly_correlated <- findCorrelation(correlation_matrix, cutoff = high_correlation_threshold)
        
        # Print the indices of highly correlated features
        print(highly_correlated)
        
        # Remove highly correlated features from the dataset
        data_reduced <- data_standardized[, -highly_correlated]
        
        # Recalculate the correlation matrix
        correlation_matrix_reduced <- cor(data_reduced)
        
        # Optionally visualize the reduced correlation matrix
        corrplot(correlation_matrix_reduced, method = "circle")
        
        # Find pairs of highly correlated features - here there are none so no more variable removal required
        highly_correlated <- findCorrelation(correlation_matrix, cutoff = high_correlation_threshold)
        
    # Lasso regularisation to identify most relevant features for linear regression
        
        # Convert predictors to matrix
        X <- as.matrix(data_reduced[, -ncol(data_reduced)])  # All columns except the last one
        
        # Convert response to vector
        y <- as.numeric(data_reduced[, ncol(data_reduced)])  # The last column
        
        # Fit Lasso model with cross-validation
        lasso_model <- cv.glmnet(X, y, alpha = 1)
        
        # Print the optimal lambda
        print(paste("Optimal Lambda:", lasso_model$lambda.min))
        
        # Plot the cross-validation curve
        plot(lasso_model)
        
        # Extract coefficients at the optimal lambda
        lasso_coefficients <- coef(lasso_model, s = "lambda.min")
        
        # Convert to a data frame for easier interpretation
        lasso_coefficients_df <- as.data.frame(as.matrix(lasso_coefficients))
        names(lasso_coefficients_df) <- c("Coefficient")
        
        # Filter out the non-zero coefficients
        relevant_features <- lasso_coefficients_df[lasso_coefficients_df$Coefficient != 0, , drop = FALSE]
        
        # Display the relevant features
        print(relevant_features)
        
        # Subset the original data to include only the selected features and the response
        data_selected <- data_imputed[, c("employment", "smoking", "chlamydia", "pop.under18", "children.low.income", "screening.breast", "lbw.x", "le.inequality")]
      
    # Cross-validation with RFE
        
        # Set predictors and response
        X <- data_selected[, -ncol(data_selected)]
        y <- data_selected$le.inequality
        
        # Define the control using a random forest selection function and 10-fold cross-validation
        control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
        
        # Run RFE with cross-validation
        rfe_result <- rfe(X, y, sizes = c(1:ncol(X)), rfeControl = control)
        
        # View the RFE result
        print(rfe_result)
        
        # Optimal number of features
        print(rfe_result$optVariables)
        
        # Plot the RFE results
        plot(rfe_result, type = c("g", "o"))
        
        # Get the selected features
        selected_features <- predictors(rfe_result)
        
        # Create a new dataset with the selected features
        data_selected <- data_selected[, c(selected_features, "le.inequality")]
        
        # View the new dataset
        head(data_selected)
        
## Machine learning ---------------
    
        
    # Split into training and testing sets
        
        # Set seed
        set.seed(123)
        
        # Create partition
        trainIndex <- createDataPartition(data_selected$le.inequality, p = .7, 
                                          list = FALSE, 
                                          times = 1)
        
        # Partition data
        dataTrain <- data_selected[trainIndex, ]
        dataTest  <- data_selected[-trainIndex, ]
        
        # Standardise training data
        
        preProcValues <- preProcess(dataTrain, method = c("center", "scale")) # subtract the mean and divide by SD, to scale centre zero, SD = 1
        dataTrain_standardized <- predict(preProcValues, dataTrain)
        
        # Apply the same transformation to the test data
        
        dataTest_standardized <- predict(preProcValues, dataTest)
        
        # Train the linear regression model using 10-fold cross-validation
        model <- train(le.inequality ~ ., data = dataTrain_standardized, method = "lm",
                       trControl = trainControl(method = "cv", number = 10))
        
        # Print the model summary
        summary(model)
        
        # Make predictions on the test set
        predictions <- predict(model, newdata = dataTest_standardized)
        
        # Evaluate the model's performance
        results <- postResample(predictions, dataTest_standardized$le.inequality)
        print(results)
        
        # Combine actual and predicted values into a dataframe
        results_df <- data.frame(
          Actual = dataTest_standardized$le.inequality,
          Predicted = predictions
        )
        
        # Plot actual vs. predicted values
        ggplot(results_df, aes(x = Actual, y = Predicted)) +
          geom_point(color = "blue", alpha = 0.5) +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
          labs(title = "Actual vs Predicted Values",
               x = "Actual Values",
               y = "Predicted Values") +
          theme_minimal()
        
        # Print the model coefficients
        # Coefficient Values: These values indicate the change in the response variable for a one-unit change in the predictor variable, holding all other variables constant
        coefficients(model$finalModel)
        summary(model$finalModel)
        
        # Residual diagnostics
        # Residuals vs. Fitted: Check for homoscedasticity (constant variance).
        # Normal Q-Q Plot: Check if residuals are normally distributed.
        # Scale-Location Plot: Check for equal spread of residuals.
        # Residuals vs. Leverage: Identify potential outliers or influential observations.
        
        par(mfrow = c(2, 2))
        plot(model$finalModel)
        
        # Save the trained model
        saveRDS(model, "linear_regression_model.rds")
        
        # To load the model later
        # loaded_model <- readRDS("linear_regression_model.rds")
        
        # # Robust regression if needed
        # 
        # # Fit a robust regression model using rlm()
        # robust_model <- rlm(le.inequality ~ ., data = dataTrain_standardized)
        # summary(robust_model)
        # 
        # # Make predictions on the test set
        # predictions <- predict(robust_model, newdata = dataTest_standardized)
        # 
        # # Evaluate the model's performance
        # results <- postResample(predictions, dataTest$le.inequality)
        # print(results)
        # 
        # # Create a dataframe with actual and predicted values
        # results_df <- data.frame(
        #   Actual = dataTest$le.inequality,
        #   Predicted = predictions
        # )
        # 
        # # Plot actual vs. predicted values
        # ggplot(results_df, aes(x = Actual, y = Predicted)) +
        #   geom_point(color = "blue", alpha = 0.5) +
        #   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
        #   labs(title = "Actual vs Predicted Values (Robust Regression)",
        #        x = "Actual Values",
        #        y = "Predicted Values") +
        #   theme_minimal()
        # 
        
        
        
        