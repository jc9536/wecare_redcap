
# 1. Generate randomization allocation directly in R ----------------------

library(tidyverse)

# Define targeted sample size
total_sample_size <- 2200

# Calculate site-specific sample sizes with 200% oversampling 
# (making sure new eligible patient in whichever strata will always get randomized)
harlem_size <- round(total_sample_size * 0.38 * 2)
kings_size <- round(total_sample_size * 0.62 * 2)

# Create a dataframe to simulate participant stratification variables for each site
dat <- expand.grid(
  site_id = c("H", "K"), # H is Harlem, K is Kings County
  screen_sex = 1:2, # 1 is Male, 2 is Female
  over_18 = 0:1, # 1 is 18 and above, 0 is 12-17
  cssrs_6a_b = 0:1 # 0 is ever made a suicide attempt, 1 is never made a suicide attempt
)

# Calculate the number of participants needed per stratum, ensuring 50/50 treatment allocation
# and adjusting for the specific size of Harlem and Kings participants
dat <- dat |> 
  group_by(site_id) |>
  mutate(n = round(case_when(
    site_id == "H" ~ harlem_size / (n() * 2),
    site_id == "K" ~ kings_size / (n() * 2),
    TRUE ~ NA_real_
  ))) |> 
  arrange(site_id, screen_sex, over_18, cssrs_6a_b) |> 
  ungroup()

# Create treatment variable the pre-assigned data frame
dat_t <- rbind(dat, dat) |> 
  mutate(treatment = c(rep(0, nrow(dat)), rep(1, nrow(dat)))) |> 
  arrange(site_id, screen_sex, over_18, cssrs_6a_b)

# Replicate each row n times
dat_t <- dat_t |> uncount(n)

# set a random seed
set.seed(02112024)

# Randomly shuffle rows within each stratum before sorting
dat_t <- dat_t %>%
  group_by(site_id, screen_sex, over_18, cssrs_6a_b) |> 
  mutate(row_order = sample(row_number())) |> 
  ungroup()

# Randomly order the treatment within each stratum
dat_t <- dat_t %>%
  arrange(site_id, screen_sex, over_18, cssrs_6a_b, row_order) |> 
  select(-row_order) |> # Remove the row_order column after sorting
  select(treatment, screen_sex, over_18, cssrs_6a_b, site_id) # reorder variables to fit with Redcap

# Check the randomization allocation
View(dat_t)
sapply(dat_t, table)
dat_t |> group_by(site_id, screen_sex, over_18, cssrs_6a_b, treatment) |> summarise(n = n()) |> View()

# Export randomization table
write_csv(dat_t, "/Users/michaelfive/Library/CloudStorage/Box-Box/WeCare/Data/docs/random_allocation.csv")

# 2. Test randomization using the redcapAPI package --------

## Blocks must be specified though (not using)

# install.packages("redcapAPI")
# 
# library(redcapAPI)
# 
# unlockREDCap(connections = c(rcon = "WeCare (Youth)"), 
#              url = "https://qa.redcap.nyu.edu/redcap_v14.0.10/API/", 
#              keyring = "4B0A08D7C9BBD4610CCF8301A1B5FB53", 
#              envir = globalenv())
# 
# 
# harlem_allocation <- allocationTable(rcon, 
#                 random = "treatment", 
#                 strata = c("screen_sex", "over_18",  "cssrs_6a_b"), 
#                 block.size = 100, 
#                 replicates = 1, 
#                 seed.dev = 12345, 
#                 seed.prod = 54321,
#                 weights = c(1,1)
#                 )
# 
# 
# harlem_dev_allocation <- harlem_allocation$dev_allocation
# sapply(harlem_dev_allocation, table)
