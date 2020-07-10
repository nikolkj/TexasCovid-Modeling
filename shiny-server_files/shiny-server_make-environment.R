#!/usr/bin/Rscript
# Start-up ----
rm(list = ls());
setwd("/home/niko/Documents/R-projects/TexasCovid-modeling/shiny-server_files/")

# Packages
require(tidyverse);
require(magrittr);
# zoo

# Load Data ----
# Full Raw Data
# Data-set: Tests, Cases and Deaths
dat = read_csv(
  file = "/home/niko/Documents/R-projects/TexasCovid-modeling/shiny-server_files/Texas-County-Main.csv",
  col_names = TRUE,
  col_types = cols(
    County = readr::col_factor(),
    Date = readr::col_date(),
    DailyCount_cases = readr::col_integer(),
    DailyDelta_cases = readr::col_integer(),
    DailyCount_tests = readr::col_integer(),
    DailyDelta_tests = readr::col_integer(),
    DailyCount_deaths = readr::col_integer(),
    DailyDelta_deaths = readr::col_integer(),
    LastUpdateDate = readr::col_date()
  ),
  na = ""
) %>% select(-LastUpdateDate) 

# Data-set: Hospitalizations and Capacity
dat = read_csv(file = "/home/niko/Documents/R-projects/TexasCovid-modeling/shiny-server_files/Texas-County-TSA.csv",
               col_names = TRUE,
               col_types = cols(
                 TSA_ID = readr::col_factor(),
                 TSA_Name = readr::col_factor(),
                 Date = readr::readr::col_integer(),
                 DailyCount_patients = readr::col_integer(),
                 DailyDelta_patients = readr::col_integer(),
                 DailyCount_beds = readr::col_integer(),
                 DailyDelta_beds = readr::col_integer(),
                 LastUpdateDate = col_date()
               ),
               na = "") %>%
  select(-LastUpdateDate)

# Population segmentation data
pop = readRDS(file = "/home/niko/Documents/R-projects/TexasCovid-modeling/shiny-server_files/texas-demographics_county-populations_segmented.RDS")

# TS Model Data
# ... see "mod_all.R"
mod_county_cases = readRDS("/home/niko/Documents/R-projects/TexasCovid-modeling/shiny-server_files/mod_DailyCount-cases.RDS") 
mod_county_tests = readRDS("/home/niko/Documents/R-projects/TexasCovid-modeling/shiny-server_files/mod_DailyCount-tests.RDS") 
mod_county_deaths = readRDS("/home/niko/Documents/R-projects/TexasCovid-modeling/shiny-server_files/mod_DailyCount-deaths.RDS") 

# Plot Objects
plot_TexasCommunities.hist = readRDS(file = "/home/niko/Documents/R-projects/TexasCovid-modeling/shiny-server_files/plot_Texas-Communities_Population-Segmentation.RDS")
plot_TexasCommunities.map = readRDS(file = "/home/niko/Documents/R-projects/TexasCovid-modeling/shiny-server_files/plot_Texas-Communities_Population-Segmentation-Map.RDS")

# Preprocessing ----
# State-level Data
dat_state = dat %>% 
  select(-County) %>% 
  arrange(Date) %>%
  group_by(Date) %>% 
  summarise_at(vars(-group_cols()), sum, na.rm = TRUE) %>%
  ungroup() %>% 
  # Calculate 7-day moving averages for DailyDelta_* data
  mutate(ma_cases = zoo::rollmean(x = DailyDelta_cases, k = 7, fill = 0, align = "right"),
         ma_tests = zoo::rollmean(x = DailyDelta_tests, k = 7, fill = 0, align = "right"),
         ma_deaths = zoo::rollmean(x = DailyDelta_deaths, k = 7, fill = 0, align = "right"),
  )

# Community-level Data
dat_pop = dat %>% 
  left_join(x = .,
            y = (pop %>% 
                   select(County, pop_group, jan1_2019_pop_est) %>%
                   rename(Population = jan1_2019_pop_est)
            ), 
            by = "County") %>%
  select(-County) %>% 
  group_by(pop_group, Date) %>% 
  summarise_at(vars(-group_cols()), sum)

# County-level Data
dat_county = dat %>% 
  group_by(County) %>%
  arrange(Date) %>% 
  nest() %>%
  ungroup() 

# TSA Data Processing 
# ... TBD

# Save Image ----
save.image(file = "/home/niko/Documents/R-projects/TexasCovid-modeling/TexasCovid-Models/shiny-server_environment.Rdata",
           compress = TRUE, version = 2)





