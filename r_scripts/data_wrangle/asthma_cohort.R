# ------------------------------------------------------------------------------
# Title: Creation asthma cohort using Oregon APAC
# Author: Ryan Gan
# Date Created: 2017-12-05
# R Version: 3.4
# ------------------------------------------------------------------------------

<<<<<<< HEAD
# This file is for filtering saba outcomes.
# And combine a asthma at-risk data set with ICD9 of asthma or saba.
# original data sets and instrunction files such as index are stored in data_original file,
# the new data sets we made are stored in data_new, which contain health and smoke
# data sets. Now the outcomes are all stored in data_new/health.

# This runs on server because of its huge computing volume.


# library
library(dplyr)
library(data.table)
library(readxl) 
library(readr)

### Prepare work

# import original data ---------------------------------------------------------
read_path <- paste0("../../../data/data_original/gan_episodes_of_care.txt") # 77 million
start_time <- Sys.time()
oregon_df <- fread(read_path, sep = "|", showProgress = T) 
stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 7 mins

write_path <- paste0('../../../data/data_original/',
                     '2013_oregon_epis_care.csv') # csv file, for future use
write_csv(oregon_df, write_path)
### ----------------------------------------------------------------------------


### Make saba index

# the ndc beta-2 agonist file are made in SQLite previously.
read_path2 <- paste0("../../../data/data_original/2013_ndc_beta2_agonists.csv")
ndc_df <- read_csv(read_path2, col_names = FALSE)

# change to nice varibale name
colnames(ndc_df) <- c("ndc_code", "brand_name", "generic_product_name", 
                      "route", "category", "drug_id")

# convert to vector
ndc_codes <- as.vector(as.matrix(ndc_df$ndc_code))

ndc_in_oregon_df <- oregon_df %>%
  filter(ndc %in% ndc_codes) # 300752, unique personkey 109483

oregon_saba_index <- ndc_in_oregon_df %>%
  select(personkey) %>%
  unique() # unique personkey, 109483

write_path <- paste0('../../../data/data_new/health/2013_oregon_saba_index.csv')
write_csv(oregon_saba_index, write_csv)
### ----------------------------------------------------------------------------



### Make data set of saba or asthma

# path and file name of data ---------------------------------------------------
read_path <- paste0("../../../data/data_original/2013_oregon_epis_care.csv")
start_time <- Sys.time()
oregon_df <- fread(read_path, sep = ",", 
                   colClasses = rep("character", 72)) 
stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 14 min

## Import ICD9 code-------------------------------------------------------------
icd9_key <- read_excel("../../../data/data_original/CMS32_DESC_LONG_SHORT_DX.xlsx") %>% 
  # rename the terrible variable names
  select(dx_code = 1, long_desc = 2, short_desc = 3)

# sort by icd9 code add row variable 
icd9_key$X <- NULL
icd9_key <- arrange(icd9_key, dx_code) %>%
  mutate(n = as.numeric(row.names(icd9_key)))

which(icd9_key$dx_code == '49300') # start of asthma is row 5206
which(icd9_key$dx_code == '49392') # end of asthma is row 5219

asthma_icd9 <- filter(icd9_key, n >= 5206 & n <= 5219) %>%
  select(dx_code)
# convert to vector
asthma_icd9 <- as.vector(as.matrix(asthma_icd9))  

## -----------------------------------------------------------------------------

## data set with saba or icd
saba_asthma_df <- oregon_df %>%
  filter(STATE == "OR") %>%
  filter(personkey %in% oregon_saba_index$personkey |
           dx1 %in% asthma_icd9 |
           dx2 %in% asthma_icd9 |
           dx3 %in% asthma_icd9 |
           dx4 %in% asthma_icd9 |
           dx5 %in% asthma_icd9 |
           dx6 %in% asthma_icd9 |
           dx7 %in% asthma_icd9 |
           dx8 %in% asthma_icd9 |
           dx9 %in% asthma_icd9 |
           dx10 %in% asthma_icd9 |
           dx11 %in% asthma_icd9 |
           dx12 %in% asthma_icd9 |
           dx13 %in% asthma_icd9) # 11358660

write_path <- paste0('../../../data/data_new/2013_oregon_atrisk_asthma.csv')
write_csv(saba_asthma_df, write_path)
=======
# load library tidyverse -----
# parallel pacakges
library(parallel) 
# tidyverse
library(tidyverse)

# setup ----
# read vector of personkeys with asthma
vector_path <- paste0("./data/health/2013-oregon_asthma_personkey.csv")
asthma_personkey <- data.table::fread(vector_path, 
                                      colClasses = "character")
# convert to vector
asthma_personkey_v <- asthma_personkey %>% 
  as_vector()

# read path for APAC dataframe
read_path <- paste0("./data/health/gan_episodes_of_care.txt")
# test path
#read_path <- paste0("./data/health/oregon_subset.txt")

# read sections dataframe -------
# there are 77069321 rows (including header) in the Oregon APAC
# I am going to read 200000 rows at a time in parallel to find unique persons
# with an asthma icd9 code in any of the dx variables
# define number of lines
read_length <- 200000
n_start <- seq(from=1, to=77069321, by=read_length)

# test set
# read_length <- 100
# n_start <- seq(from=1, to=10000, by = read_length)

# read first line and get column names
df_col_names <- data.table::fread(read_path, nrows = 0, header = T, 
                                  colClasses = rep("character", 72))
col_names <- colnames(df_col_names) # output column names
# remove 
rm(df_col_names)

# set up parallel computing ----
# find number of processing nodes (cores)
cores <- detectCores() # should be 16 per node
# make cluster/node
cl <- makeCluster(cores)

# load packages on each processor of the node/cluster
clusterCall(cl, function() library(tidyverse))
# export read path to each core
clusterExport(cl, c("read_path", "read_length", "n_start", "col_names", 
  "asthma_personkey_v"), envir = .GlobalEnv)

# register start time
start_time <- proc.time()
# run in parallel -----
# read in rows of the oregon file
df_list <- parLapply(cl, n_start, function(x){
  read_df <- data.table::fread(read_path, sep = "|", header = F,
          # read set number of rows and skip what has already been read
          nrows=read_length, skip=x, colClasses = rep("character", 72))
  # assign column names
  colnames(read_df) <- col_names
  # filter df to subset of asthma icd9s in any dx column
  out_df <- read_df %>% 
    filter(personkey %in% asthma_personkey_v)
  # return vector of personkeys with asthma ids
  return(out_df)
  } # end function
) # end parsapply

# stop cluster
stopCluster(cl)
# find time process took
stop_time <- proc.time() - start_time
stop_time

# create dataframe of subjects with at least one diagnosis of asthma ----
# bind the subsets of lists in to one dataframe
asthma_cohort <- df_list %>% 
  bind_rows()

# print first keys
head(asthma_cohort[,1:10])
# save as csv ----
write_path <- paste0("./data/health/2013-oregon_asthma_cohort.csv")
data.table::fwrite(asthma_cohort, write_path)

>>>>>>> upstream/development
