# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tidyverse", "REDCapR", "rlang", "flextable",
               "officer", "officedown"), # packages that your targets need to run
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
      redcap_uri = youth_url,
      token = youth_api_token
    )$data),
    force = T
    ),
  
  # Caregiver data
tar_force(
    dat_caregiver_raw,
    suppressMessages(redcap_read(
      redcap_uri = caregiver_url,
      token = caregiver_api_token
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

  # Process merged data for reporting purposes
  tar_target(
    dat_report,
    process_report_data(dat_merged)
  ),

# 4. Export data ----------------------------------------------------------

  # Export youth data
  tar_target(
    export_dat_youth_cleaned,
    write_csv(dat_youth_cleaned, file.path(box_path, "cleaned/dat_youth_cleaned.csv"))
  ),

  # Export caregiver data
  tar_target(
    export_dat_caregiver_cleaned,
    write_csv(dat_caregiver_cleaned, file.path(box_path, "cleaned/dat_caregiver_cleaned.csv"))
  ),

  # Export merged data
  tar_target(
    export_dat_merged,
    write_csv(dat_merged, file.path(box_path, "cleaned/dat_merged.csv"))
  ),

  # Export data for reporting
  tar_target(
    export_dat_report_cleaned,
    write_csv(dat_report, file.path(box_path, "cleaned/dat_report.csv"))
  ),

# 5. Create tables and figures ----------------------------------------------------------
  tar_target(t_enrollment, tab_enrollment(dat_report)),
  tar_target(t_weekly_enrollment, tab_weekly_enrollment(dat_report)),
  tar_target(t_recruitment, tab_recruitment(dat_report)),
  tar_target(t_assignment, tab_assignment(dat_report)),
  tar_target(t_trigger_rmp, tab_trigger_rmp(dat_report)),
  tar_target(t_demo, tab_demo(dat_report)),
  tar_target(t_monthly_enrollment_preprocessed, tab_monthly_enrollment_preprocess(dat_report)), # intermediate preprocess step
  tar_target(t_monthly_enrollment, tab_monthly_enrollment(t_monthly_enrollment_preprocessed)),
  tar_target(p_monthly_enrollment, fig_monthly_enrollment(t_monthly_enrollment_preprocessed)),

# 6. Build report ----------------------------------------------------------
  
  # Export data for reporting
  tar_render(
    name = build_report,
    path = here::here("inst/progress_report.Rmd"),
    output_file = here::here("output", "progress_report.docx")
  )

)
