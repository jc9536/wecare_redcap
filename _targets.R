# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tidyverse", "REDCapR", "lubridate"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Box path (please change if you need to run the pipeline locally)
box_path = "/Users/michaelfive/Library/CloudStorage/Box-Box/WeCare/Data"

# Replace the target list below with your own:
list(

# 1. Load data from API ---------------------------------------------------

  # Youth data
  tar_force(
    dat_youth_raw,
    suppressMessages(redcap_read(
      redcap_uri = youth_url_qa,
      token = youth_api_token_qa
    )$data),
    force = T
    ),
  
  # Caregiver data
tar_force(
    dat_caregiver_raw,
    suppressMessages(redcap_read(
      redcap_uri = caregiver_url_qa,
      token = caregiver_api_token_qa
    )$data),
    force = T
    ),


# 2. Cleaning data --------------------------------------------------------

  # Clean youth data
  tar_target(
    dat_youth_cleaned,
    clean_youth(dat_youth_raw)
  ),

  # Clean caregiver data
  tar_target(
    dat_caregiver_cleaned,
    clean_caregiver(dat_caregiver_raw)
  ),

# 3. Export data ----------------------------------------------------------
  
  # Merge caregiver and youth data
  tar_target(
    dat_merged,
    merge_caregiver_youth(dat_caregiver_cleaned, dat_youth_cleaned)
  ),

# 4. Export data ----------------------------------------------------------

  # Export youth data
  tar_target(
    export_dat_youth_cleaned,
    write_csv(dat_youth_cleaned, file.path(box_path, "cleaned/dat_youth_cleaned.csv"))
  ),

  # Export youth data
  tar_target(
    export_dat_caregiver_cleaned,
    write_csv(dat_caregiver_cleaned, file.path(box_path, "cleaned/dat_caregiver_cleaned.csv"))
  ),

  # Export youth data
  tar_target(
    export_dat_merged,
    write_csv(dat_merged, file.path(box_path, "cleaned/dat_merged.csv"))
)
)
