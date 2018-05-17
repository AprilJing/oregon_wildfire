# ------------------------------------------------------------------------------
# Title: Asthma place of service time-stratified case-crossover
# Author: Ryan Gan
# Date Created: 2018-04-16
# ------------------------------------------------------------------------------

# Description: This script subsets the asthma claims to specific service places
# and creates time-stratified case-crossover dataframes


# library ----
library(tidyverse)
library(case.crossover)
library(parallel)

# asthma place of service ----

# read in place of service csv file
place_of_service <- read_csv("./data/health/2013-oregon_pos.csv")

pos_subset <- c("11", "20", "21", "22", "23", "41")

# read in asthma icd9 vector
load("./data/health/outcome_list.RData")

asthma_icd9 <- outcome_icd9_list$asthma

# read in asthma claims
asthma_claims <- read_csv("./data/health/2013_oregon_asthma_claims.csv") %>% 
  # filter to date of PM2.5 data (wildfire season)
  filter(fromdate >= "2013-05-01" & fromdate <= "2013-09-30") %>% 
  # filter to oregon
  filter(STATE == "OR") %>% 
  # filter to asthma claims of a primary diagnosis only
  filter(dx1 %in% asthma_icd9) %>% 
  # filter to place of service i want to analyze
  filter(pos %in% pos_subset) %>% 
  # left join to place of service names
  left_join(place_of_service, by = "pos") %>% 
  # split by list
  split(.$pos)

# transform pos_subset to list
asthma_list <- as.list(asthma_claims)

# set up cores
cl <- makeCluster(6)

# load packages on each processor of the node/cluster
clusterCall(cl, function() c(library(tidyverse), library(case.crossover)))

# export new set of objects to global objects to cluster
clusterExport(cl, c("asthma_list"), 
              envir = .GlobalEnv)

# parallel creation of casecrossover list -----
# start time
start <- Sys.time()

#asthma_cc_pos_list <- parLapply(cl, asthma_list, function(x){
asthma_cc_pos_list <- lapply(asthma_list, function(x){
    pos_df <- x %>% 
    # find the first observation and first claim line of each person 
    group_by(personkey) %>%
    arrange(fromdate, line) %>%
    filter(row_number()==1) %>% 
    ungroup() %>% 
    mutate(age = 2013 - yob)
    
    ts_df <- time_stratified(data = pos_df, id = "personkey", 
      covariate = c("patid", "gender", "age", "MSA", "ZIP", "pos", "dx1",
                  "service_place"), 
      admit_date = "fromdate", 
      start_date = "2013-05-01", end_date = "2013-09-30", interval = 7)
})

stop <- Sys.time()
time <- stop - start
print(time)


# close cores 
stopCluster(cl)

# save casecross list as r data object
save(asthma_cc_pos_list, file = "./data/health/asthma_cc_pos_list.RData")
