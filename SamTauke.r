###############################################################################################
# PepsiCo Data Challenge
# Sam Tauke 10/14/20
#
# This code file takes in the provided data from excel, cleans and merges it, and runs 
# the model.  The model is a linear regression model of the predicted score on
# a number of explanatory factors at the assessment_type/variety level.
# Additionally, the model allows for heterogeneous effects across different different site_id
# by including fixed effects for the different sites. This model only considers assessments
# done between the date of sowing and the date of harvesting, all others are assumed to have
# a data error that makes them unreliable. 

# The adjusted r-squared values indicate that the model is fairly successful for all assessment
# type/variety combinations except for variety "B"/assessment type "C - D".  This is clearly
# a weakness in the model that should be investigated further.  I note that while the use of
# a linear model like this is a simplistic approach, it is a solid foundation for future iteration
# to a more accurate model.

# Logistical Note: This file assumes the nyas-challenge-2020-data.xlsx is located in the same
# directory as the code file and that the working directory have been defined.


###############################################################################################
rm(list = ls(all = TRUE)) # clear all variables in the environment



library(ggpubr)
library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(caTools)
library(sjmisc)
library(tidyverse)
library(broom)



# Read in Data ------------------------------------------------------------

crop_grain_raw <- read_xlsx("nyas-challenge-2020-data.xlsx",
                            sheet = "Crop and Grain Data",
                            col_names = c("id","site_id","growth_stage","variety",
                                          "assessment_date_raw","assessment_type","assessment_score_raw"),
                            skip = 1)

weather_raw <- read_xlsx("nyas-challenge-2020-data.xlsx",
                         sheet = "Weather Data",
                         col_names = c("site_id","date_raw","weather_var_a",
                                       "weather_var_b","weather_var_c","weather_var_d",
                                       "weather_var_e","weather_var_f"),
                         skip = 1)


site_raw <- read_xlsx("nyas-challenge-2020-data.xlsx",
                         sheet = "Site Data",
                      col_names = c("site_id","latitude","elevation","sowing_date_raw",
                         "harvest_date_raw","soil_parameter_a","soil_parameter_b",
                         "amount_fertilizer"),
                      skip = 1)




# Clean Crop and Grain Data -------------------------------------------
#  Clean up dates and assessment score, drop missing assessment scores, pull out site and harvest year

crop_clean <- crop_grain_raw %>% 
  mutate(
    assessment_date = as.Date(as.character(assessment_date_raw)),
    assessment_score = as.numeric(assessment_score_raw),
    assessment_year = year(assessment_date),
    site = substr(site_id,6,6),
    harvest_year = as.numeric(substr(site_id,13,17))
  ) %>% 
  filter(!is.na(assessment_score)) %>% 
  arrange(site,assessment_year) 

#  Clean Weather Data -------------------------------------------
#  clean up dates, pull out site and harvest year
weather_clean <- weather_raw %>% 
  mutate(
    weather_date = as.Date(as.character(date_raw)),
    site = substr(site_id,6,6),
    harvest_year = as.numeric(substr(site_id,8,11))
  ) 





# Clean Site Data ---------------------------------------------------------
# Clean up dates, extract site and harvest year

site_clean <- site_raw %>% 
  mutate(
    sowing_date = as.Date(as.character(sowing_date_raw)),
    harvest_date = as.Date(as.character(harvest_date_raw)),
    site = substr(site_id,6,6),
    harvest_year = as.numeric(substr(site_id,8,12))
  )  
  





# Summarize Data At Site Level --------------------------------------------
# look at density of variety and site_id, use for correlation experimenting


crop_summary <-  crop_clean %>% 
  group_by(site_id,variety) %>% 
  summarise(
    num_obs = n(),
    growth_score_corr = cor(growth_stage,assessment_score)
  ) %>% 
  ungroup()



# Summarize Data on Tests -------------------------------------------------
# Look at density of assessment_type/variety


test_summary <- crop_clean %>% 
  filter(grepl("Site A Year 2015",site_id,ignore.case = T)) %>%
  group_by(assessment_type,variety) %>% 
  summarise(
    min_score = min(assessment_score),
    max_score = max(assessment_score),
    earliest_date = min(assessment_date),
    latest_date = max(assessment_date),
    min_g_stage = min(growth_stage),
    max_g_stage = max(growth_stage)
  )


# Find Average Weather for each site_id starting on day after sowing date.
# Average weather is a sufficient statistic for the cumulative weather effect from sowing until assessment dates
weather_avg <- weather_clean %>% 
  group_by(site_id) %>% 
  mutate(
    group_row_num = row_number(),
    avg_weather_a = runmean(weather_var_a,k=group_row_num),
    avg_weather_b = runmean(weather_var_b,k=group_row_num),
    avg_weather_c = runmean(weather_var_c,k=group_row_num),
    avg_weather_d = runmean(weather_var_d,k=group_row_num),
    avg_weather_e = runmean(weather_var_e,k=group_row_num),
    avg_weather_f = runmean(weather_var_f,k=group_row_num)
    ) %>% 
  ungroup()


# Define Function to Slice Up Data and Run Regression ---------------------


# Combine crop, weather, and site data to build input for regression
reg_data <- crop_clean %>% 
  left_join(weather_avg %>% select(site,harvest_year,weather_date,contains("avg_weather")),
            by=c("assessment_date"="weather_date","site","harvest_year")) %>% 
  left_join(site_clean %>% select(site,harvest_year,sowing_date,harvest_date,soil_parameter_a,soil_parameter_b,amount_fertilizer),
            by=c("site","harvest_year")) %>% 
  filter(assessment_date>sowing_date & assessment_date<=harvest_date)


reg_machine_m <- function(test){
  temp_data <- reg_data %>% 
    filter(variety=="M") %>% 
    filter(
      assessment_type==test)
  
  machine_model <- lm(assessment_score ~ growth_stage + avg_weather_a + avg_weather_b +
                      avg_weather_c + avg_weather_d + avg_weather_e + avg_weather_f + 
                      soil_parameter_a + soil_parameter_b + amount_fertilizer + factor(site_id),
                    data = temp_data,
                    na.action = na.exclude)
  
  temp_data <- temp_data %>% 
    mutate(
      predicted_score = fitted(machine_model),
      residuals = resid(machine_model),
      adj_rsquare = summary(machine_model)$adj.r.squared
    )
  
  
return(temp_data)
  
}

assessment_list_m <- reg_data %>% 
  filter(variety=="M") %>% 
  distinct(assessment_type) %>% 
  pull(assessment_type)



results_m <- lapply(assessment_list_m,reg_machine_m) %>% 
  bind_rows()


# Regression for Test B ---------------------------------------------------

reg_machine_b <- function(test){
  temp_data <- reg_data %>% 
    filter(variety=="B") %>% 
    filter(
      assessment_type==test)
  
  machine_model <- lm(assessment_score ~ growth_stage + avg_weather_a + avg_weather_b +
                        avg_weather_c + avg_weather_d + avg_weather_e + avg_weather_f + 
                        soil_parameter_a + soil_parameter_b + amount_fertilizer + factor(site_id),
                      data = temp_data,
                      na.action = na.exclude)
  
  temp_data <- temp_data %>% 
    mutate(
      predicted_score = fitted(machine_model),
      residuals = resid(machine_model),
      adj_rsquare = summary(machine_model)$adj.r.squared
    )
  
  
  return(temp_data)
  
}

assessment_list_b <- reg_data %>% 
  filter(variety=="B") %>% 
  distinct(assessment_type) %>% 
  pull(assessment_type)

results_b <- lapply(assessment_list_b,reg_machine_b) %>% 
  bind_rows()


#Combine results to be exported

combined_results <- results_m %>% 
  bind_rows(results_b)


#Merge Results onto Original Dataset


export_data <- crop_grain_raw %>% 
  left_join(combined_results %>% select(id,predicted_score),by="id") %>% 
  mutate(
    predicted_score = ifelse(is.na(predicted_score),"Incomplete Data or Out of Allowable Dates",predicted_score)
  )


write_csv(export_data,"predicted_scores.csv")




