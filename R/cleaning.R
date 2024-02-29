# library(tidyverse)
# library(REDCapR)

# tar_load_everything()

# Function to clean youth data
clean_youth <- function(dat_youth_raw){
  
  dat_youth_cleaned <- dat_youth_raw |> 
    janitor::clean_names()
  
  return(dat_youth_cleaned)
}

# Function to clean caregiver data
clean_caregiver <- function(dat_caregiver_raw){
  
  dat_caregiver_cleaned <- dat_caregiver_raw |> 
    janitor::clean_names()
  
  return(dat_caregiver_cleaned)
}
