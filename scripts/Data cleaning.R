#Data cleaning
#Housekeeping
setwd()
#Load required packages
library(readxl)
library(writexl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)

# Load Ottawa  data
clim2021<- read.csv("en_climate_daily_ON_6106001_2021_P1D (1).csv", header = TRUE)
clim2022<- read.csv("en_climate_daily_ON_6106001_2022_P1D.csv", header = TRUE)
soil_tmp_5cm <- read.csv("Soildata_05cm.csv", header = TRUE)  

#Join the first two dataframes 
clim_ottawa_21_22 <- rbind(clim2021, clim2022)

#Create subset with required columns only 
clim_ottawa_21_22<- clim_ottawa_21_22 %>%
  select(Station.Name, Date.Time, Max.Temp...C., Min.Temp...C., Total.Rain..mm., Total.Precip..mm., Total.Snow..cm.) %>%
  rename(station = Station.Name, date = Date.Time, max_temp = Max.Temp...C., min_temp = Min.Temp...C., total_rain = Total.Rain..mm., total_precip = Total.Precip..mm., total_snow = Total.Snow..cm.)
clim_ottawa_21_22<- filter(clim_ottawa_21_22, date > "2021-08-31", date < "2022-02-01")

# Ensure same Date-Time format for soil data 
soil_tmp_5cm <- soil_tmp_5cm %>%
  separate(timestamp, into = c("date", "time"), sep = " ")
#Average daily soil temps 
soil_tmp_daily <- soil_tmp_5cm %>%
  group_by(date) %>% # Group by date
  summarise(mean_soiltemp = mean(soil.temp)) # Summarize with desired function

#Merge clim and soil temp data 
clim_soil_merged <- merge(clim_ottawa_21_22, soil_tmp_daily, by="date")
#Make sure only numeric columns are kept for further analysis
clim_soil_merged <- clim_soil_merged %>%
  select(where(is.numeric))