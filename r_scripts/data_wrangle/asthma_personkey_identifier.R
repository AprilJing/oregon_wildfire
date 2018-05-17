# ------------------------------------------------------------------------------
# Title: Finding unique persons with a diagnosis of asthma in Oregon APAC
# Author: Ryan Gan
# Date Created: 2017-12-05
# R Version: 3.4
# ------------------------------------------------------------------------------

# load libraries -----
# parallel pacakges
library(parallel) # package for parallel computations
# tidyverse
library(tidyverse)

# assign read_path for oregon data and load asthma icd9 -----
read_path <- paste0("./data/health/gan_episodes_of_care.txt")

# read in Rdata icd9 outcome vectors and limit to asthma icd9s and saba ndcs
load("./data/health/outcome_list.RData")
# subset to asthma icd9
asthma_icd9 <- pluck(outcome_icd9_list, "asthma")
# print asthma_icd9 as a check
print(asthma_icd9)

# read sections dataframe -------
# there are 77069321 rows (including header) in the Oregon APAC
# I am going to read 200000 rows at a time in parallel to find unique persons
# with an asthma icd9 code in any of the dx variables
# define number of lines
read_length <- 200000
n_start <- seq(from=1, to=77069321, by = read_length)

# read first line and get column names
df_col_names <- data.table::fread(read_path, nrows = 0, header = T, 
                                  colClasses = rep("character", 72))
col_names <- colnames(df_col_names) # output column names

# set up parallel computing ----
# find number of processing nodes (cores)
cores <- detectCores() # should be 16 per node
# make cluster/node
cl <- makeCluster(cores)

# load packages on each processor of the node/cluster
clusterCall(cl, function() library(tidyverse))
# export read path to each core
clusterExport(cl, 
  c("read_path", "read_length", "n_start", "col_names", "asthma_icd9"), 
  envir = .GlobalEnv)

start_time <- proc.time()
# run in parallel -----
# read in rows of the oregon file
personkey_list <- parSapply(cl, n_start, function(x){
  df <- data.table::fread(read_path, sep = "|", header = F,
                          # read set number of rows and skip what has already been read
                          nrows=read_length, skip=x, 
                          colClasses = rep("character", 72))
  # assign column names
  colnames(df) <- col_names
  # filter df to subset of asthma icd9s in any dx column
  asthma_id <- df %>% 
    filter_at(vars(contains("dx")), any_vars(. %in% asthma_icd9)) %>% 
    select(personkey) %>% 
    as_vector() 
  # return vector of personkeys with asthma ids
  return(asthma_id)
  } # end function
) # end parsapply

# stop cluster
stopCluster(cl)
# find time process took
stop_time <- proc.time() - start_time
stop_time

# create csv of personkey IDs with at least one diagnosis of asthma ----
# unlist the list of personkeys with asthma and find the unique observations
asthma_personkey <- as_data_frame(unique(unlist(personkey_list))) %>% 
  rename(personkey = value)

# print first keys
head(asthma_personkey)
# save as csv ----
write_path <- paste0("./data/health/2013-oregon_asthma_personkey.csv")
data.table::fwrite(asthma_personkey, write_path)
