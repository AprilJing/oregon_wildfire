# ------------------------------------------------------------------------------
# Title: Subsetting asthma cohort to fire season 
# Author: Ryan Gan
# Date Created: 2017-12-08
# ------------------------------------------------------------------------------

# Description: This script subsets the asthma cohort to further reduce the 
# number of observations to only asthma or saba observations to reduce the 
# number of events that need to be evaluated. I also add/modify this dataset


# library ----
library(tidyverse)

# read asthma cohort dataset and affiliated files ------
# read path
read_path <- "./data/health/2013-oregon_asthma_cohort.csv"

# load outcomes vectors
load("./data/health/outcome_list.RData")
# subset asthma and saba vector
asthma_icd9 <- pluck(outcome_icd9_list, "asthma")
saba_ndc <- pluck(outcome_icd9_list, "saba")

# read in place of service csv file
place_of_service <- read_csv("./data/health/2013-oregon_pos.csv")

# read in asthma cohort (reading a million rows for now)
asthma_fire_season_cohort <- data.table::fread(read_path, 
  # read options
  header = T, #nrows = 100000, # removing nrows read after testing
  colClasses = rep("character", 72)) %>% 
  # filter to oregon
  filter(STATE == "OR") %>% 
  # replace *NULL* with NA
  mutate_all(funs(replace(., .== "*NULL*" | . == "", NA))) %>% 
  # create visit type indicator 
  mutate(visit_type = as.factor(
    case_when((dx1 %in% asthma_icd9) ~ "dx_asthma_primary",
      # asthma but not as a primary
      !(dx1 %in% asthma_icd9) & (dx2 %in% asthma_icd9 | dx3 %in% asthma_icd9 | 
      dx4 %in% asthma_icd9 | dx5 %in% asthma_icd9 | dx6 %in% asthma_icd9 | 
      dx7 %in% asthma_icd9 | dx8 %in% asthma_icd9 | dx9 %in% asthma_icd9 | 
      dx10 %in% asthma_icd9 | dx11 %in% asthma_icd9 | dx12 %in% asthma_icd9 | 
      dx13 %in% asthma_icd9) ~ "dx_asthma_not_primary",
      # other dx 
      !(dx1 %in% asthma_icd9 | dx2 %in% asthma_icd9 | dx3 %in% asthma_icd9 | 
      dx4 %in% asthma_icd9 | dx5 %in% asthma_icd9 | dx6 %in% asthma_icd9 | 
      dx7 %in% asthma_icd9 | dx8 %in% asthma_icd9 | dx9 %in% asthma_icd9 | 
      dx10 %in% asthma_icd9 | dx11 %in% asthma_icd9 | 
      dx12 %in% asthma_icd9 | dx13 %in% asthma_icd9) & is.na(ndc) ~ "dx_other",
      # ndc code
      (ndc %in% saba_ndc) ~ "pharm_saba",
      !is.na(ndc) & !(ndc %in% saba_ndc) ~ "pharm_other")),
    # adding names of metro area
    metroarea = case_when(
      MSA == 13460 ~ "Bend-Redmond",
      MSA == 18700 ~ "Corvallis",
      MSA == 21660 ~ "Eugene-Springfield",
      MSA == 32780 ~ "Medford",
      MSA == 38900 ~ "Portland-Vancouver-Beaverton",
      MSA == 41420 ~ "Salem",
      MSA == 41999 ~ "Not in MSA"),
    los = as.numeric(los),
    # create age variable
    age = 2013 - as.numeric(yob),
    # code in race text to numeric race
    race_factor = case_when(race == 1 ~ "White",
                            race == 2 ~ "Black",
                            race == 3 ~ "American Indian",
                            race == 4 ~ "Asian",
                            race == 5 ~ "Pacific Islander",
                            race == 6 ~ "Other", 
                            race == 7 ~ "Two or more",
                            race == 9 ~ "Unknown"),
    ethnicity = case_when(ethn == "1" ~ "Hispanic",
                          ethn == "2" ~ "Not Hispanic",
                          ethn == "3" ~ "Unknown")) %>% 
  # convert date variables to dates
  mutate_at(vars(contains("date")), funs(as.Date(.,format="%Y-%m-%d"))) %>%
  # join in place of service text
  left_join(place_of_service, by = "pos") %>% 
  # simpler place of service grouping
  mutate(pos_simple = as.factor(case_when(pos == "01" ~ "Pharmacy",
    pos == "03" ~ "School",
    pos == "11" ~ "Office",
    pos == "20" ~ "Urgent Care", 
    pos == "21" ~ "Inpatient Hospital",
    pos == "22" ~ "Outpatient Hospital",
    pos == "23" ~ "Emergency Room Hospital",
    pos == "41" | pos == "42" ~ "Ambulance",
    pos == "71" | pos == "72" ~ "Public Health Clinic",
    pos == "49" ~ "Independent Clinic", 
    TRUE ~ "Other"))) %>% 
  # filter todate to wildfire season (May to October)
  filter(todate >= "2013-05-01" & todate < "2013-10-01") %>% 
  # filter to asthma or saba observations
  filter(visit_type == "dx_asthma_primary" | 
         visit_type == "dx_asthma_not_primary" |
         visit_type == "pharm_saba") 

# write csv ----
write_path <- paste0("./data/health/2013-oregon_asthma_fireseason_cohort.csv")
data.table::fwrite(asthma_fire_season_cohort, write_path)


# saving this code for prep in analysis script
  # # coding outcomes classified
  # mutate(outcome_class = as.factor(case_when(
  #   visit_type == "dx_asthma_primary" & pos == 23 ~ "asthma_emergency",
  #   visit_type == "dx_asthma_primary" & pos == 20 ~ "asthma_urgentcare",
  #   visit_type == "dx_asthma_primary" & pos == 21 ~ "asthma_inpatient",
  #   visit_type == "dx_asthma_primary" & pos == 41 ~ "asthma_ambulance",
  #   visit_type == "pharm_saba" ~ "inhaler_fill")))


