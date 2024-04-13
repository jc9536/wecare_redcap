# library(tidyverse)
# library(REDCapR)
# tar_load_everything()

# Function to clean youth data
clean_youth <- function(dat_youth_raw){
  
  dat_youth_cleaned <- dat_youth_raw |> 
    janitor::clean_names() |> 
    filter(!is.na(site_id)) # omit the preloaded rows with no data (site_id is the required first field)
  
  # coalesce pre-ICF variables
  dat_youth_cleaned <- dat_youth_cleaned |> 
    mutate(
      ps_hear_more = coalesce_columns(dat_youth_cleaned, "ps[0-9]{2}y_", "hear_more")|> pull(1),
      ps_willing_to_contact = coalesce_columns(dat_youth_cleaned, "ps[0-9]{2}y_", "willing_to_contact")|> pull(1),
      ps_decline_reason = coalesce_columns(dat_youth_cleaned, "ps[0-9]{2}y_", "decline_reason")|> pull(1),
      ps_youth_name = coalesce_columns(dat_youth_cleaned, "ps[0-9]{2}y_", "youth_name")|> pull(1),
      ps_signature = coalesce_columns(dat_youth_cleaned, "ps[0-9]{2}y_", "signature")|> pull(1),
      ps_date = coalesce_columns(dat_youth_cleaned, "ps[0-9]{2}y_", "date")|> pull(1),
      .after = initial_questions_complete
    ) |> 
    # remove the original un-coalesced variables
    select(-matches("^(harlem|kings)_ps\\d{2}y.*")) |> 
    # coalesce prescreening contact form completion indicators
    mutate(contact_form_youth_complete = coalesce_columns(dat_youth_cleaned, ".*prescreening_contact_form.*", "complete") |> pull(1),
           .before = first_name) |> 
    # remove the original un-coalesced variables
    select(-matches("prescreening_contact_form"))
  
  # coalesce ICF variables
  dat_youth_cleaned <- dat_youth_cleaned |> 
    mutate(
      icf_name_1 = coalesce_columns(dat_youth_cleaned, "icf[0-9]{2}y_", "name_1") |> pull(1),
      icf_name_2 = coalesce_columns(dat_youth_cleaned, "icf[0-9]{2}y_", "name_2") |> pull(1),
      icf_nih_share = coalesce_columns(dat_youth_cleaned, "icf[0-9]{2}y_", "nih_share") |> pull(1),
      icf_name_3 = coalesce_columns(dat_youth_cleaned, "icf[0-9]{2}y_", "name_3") |> pull(1),
      icf_nih_first_name = coalesce_columns(dat_youth_cleaned, "icf[0-9]{2}y_", "first_name") |> pull(1),
      icf_nih_middle_name = coalesce_columns(dat_youth_cleaned, "icf[0-9]{2}y_", "middle_name") |> pull(1),
      icf_nih_last_name = coalesce_columns(dat_youth_cleaned, "icf[0-9]{2}y_", "last_name") |> pull(1),
      icf_nih_dob = coalesce_columns(dat_youth_cleaned, "icf[0-9]{2}y_", "dob") |> pull(1),
      icf_nih_sex = coalesce_columns(dat_youth_cleaned, "icf[0-9]{2}y_", "sex") |> pull(1),
      icf_nih_city = coalesce_columns(dat_youth_cleaned, "icf[0-9]{2}y_", "city") |> pull(1),
      .after = eligibility_survey_complete
    ) |> 
    # remove the original un-coalesced variables
    select(-matches("^(harlem|kings)_icf\\d{2}y.*")) |> 
    # coalesce ICF completion indicators
    mutate(informed_consent_form_youth_complete = coalesce_columns(dat_youth_cleaned, ".*subject_information_and_informed.*", "complete") |> pull(1),
           .before = sw_pause_id) |> 
    # remove the original un-coalesced variables
    select(-matches("subject_information_and_informed")) |> 
    as_tibble()
  
  return(dat_youth_cleaned)
}

# Function to clean caregiver data
clean_caregiver <- function(dat_caregiver_raw){
  
  # auto-clean variable names
  dat_caregiver_cleaned <- dat_caregiver_raw |> 
    janitor::clean_names()
  
  # coalesce pre-ICF variables
  dat_caregiver_cleaned <- dat_caregiver_cleaned |> 
    
    # coalesce the form language variables across sites
    mutate(p_screen_language = coalesce(p_screen_language_harlem, p_screen_language_kings),
           .after = p_screen_language_kings) |> 
    # remove the original un-coalesced variables
    select(-c(p_screen_language_harlem, p_screen_language_kings)) |>
    
    # coalesce prescreening contact form variables
    mutate(
      p_ps_hear_more = coalesce_columns(dat_caregiver_cleaned, "ps[0-9]{2}_", "hear_more") |> pull(1),
      p_ps_willing_to_contact = coalesce_columns(dat_caregiver_cleaned, "ps[0-9]{2}_", "willing_to_contact") |> pull(1),
      p_ps_decline_reason = coalesce_columns(dat_caregiver_cleaned, "ps[0-9]{2}_", "decline_reason") |> pull(1),
      p_ps_caregiver_name = coalesce_columns(dat_caregiver_cleaned, "ps[0-9]{2}_", "caregiver_name") |> pull(1),
      p_ps_youth_name = coalesce_columns(dat_caregiver_cleaned, "ps[0-9]{2}_", "youth_name") |> pull(1),
      p_ps_signature = coalesce_columns(dat_caregiver_cleaned, "ps[0-9]{2}_", "signature") |> pull(1),
      p_ps_date = coalesce_columns(dat_caregiver_cleaned, "ps[0-9]{2}_", "date") |> pull(1),
      .after = initial_questions_complete
    ) |> 
    # remove the original un-coalesced variables
    select(-matches("^(harlem|kings)_ps\\d{2}.*")) |>
    
    # coalesce prescreening contact form completion indicators
    mutate(contact_form_parent_complete = coalesce_columns(dat_caregiver_cleaned, ".*prescreening_contact_form.*", "complete") |> pull(1),
           .before = p_first_name) |> 
    # remove the original un-coalesced variables
    select(-matches("prescreening_contact_form"))
  
  # rename icf18 participant name fields so that the same suffix is matched across all icfs
  # _1: Caregiver: I voluntarily agree to participate in this study.
  # _2: Caregiver: I voluntarily agree for my child to participate in this study.
  # _3: Youth: I voluntarily agree to participate in this study.
  # _4: Person obtaining consent name
  # _5: NIH data sharing consent caregiver name
  dat_caregiver_cleaned <- dat_caregiver_cleaned |> 
    # rename new = old
    rename(
      harlem_icf18_name_5 = harlem_icf18_name_3,
      harlem_icf18_name_4 = harlem_icf18_name_2,
      kings_icf18_name_5 = kings_icf18_name_3,
      kings_icf18_name_4 = kings_icf18_name_2,
      
      harlem_icf18_name_5_spa = harlem_icf18_name_3_spa,
      harlem_icf18_name_4_spa = harlem_icf18_name_2_spa,
      kings_icf18_name_5_spa = kings_icf18_name_3_spa,
      kings_icf18_name_4_spa = kings_icf18_name_2_spa,
      
      harlem_icf18_name_5_fre = harlem_icf18_name_3_fre,
      harlem_icf18_name_4_fre = harlem_icf18_name_2_fre,
      kings_icf18_name_5_hai = kings_icf18_name_3_hai,
      kings_icf18_name_4_hai = kings_icf18_name_2_hai
    )
  
  # coalesce ICF variables
  dat_caregiver_cleaned <- dat_caregiver_cleaned |> 
    mutate(
      p_icf_name_1 = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "name_1") |> pull(1),
      p_icf_name_2 = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "name_2") |> pull(1),
      p_icf_name_3 = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "name_3") |> pull(1),
      p_icf_name_4 = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "name_4") |> pull(1),
      p_icf_nih_share = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "nih_share") |> pull(1),
      p_icf_name_5 = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "name_5") |> pull(1),
      p_icf_nih_first_name = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "first_name") |> pull(1),
      p_icf_nih_middle_name = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "middle_name") |> pull(1),
      p_icf_nih_last_name = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "last_name") |> pull(1),
      p_icf_nih_dob = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "dob") |> pull(1),
      p_icf_nih_sex = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "sex") |> pull(1),
      p_icf_nih_city = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "city") |> pull(1),
      .after = eligibility_screen_complete
    ) |> 
    # remove the original un-coalesced variables
    select(-matches("^(harlem|kings)_icf\\d{2}.*")) |> 
    # coalesce ICF completion indicators
    mutate(informed_consent_form_parent_complete = coalesce_columns(dat_caregiver_cleaned, ".*subject_information_and_informed.*", "complete") |> pull(1),
           .before = p_sw_pause_id) |> 
    # remove the original un-coalesced variables
    select(-matches("subject_information_and_informed")) |> 
  
  # rename completion indicators with the p_ prefix
    rename_with(~ paste0("p_", .x), ends_with("_complete") | starts_with("gf_")) |> 
    rename(
      p_wecare_id = wecare_id,
      p_over_18 = over_18,
      p_over_12 = over_12
    ) |> 
    as_tibble()
  
  return(dat_caregiver_cleaned)
}

# Function to merge youth and caregiver data
merge_caregiver_youth <- function(dat_caregiver_cleaned, dat_youth_cleaned){
  
  # merge data (no caregiver data unless youth data is available)
  dat_merged <- dat_caregiver_cleaned |> 
    right_join(dat_youth_cleaned, by = c("site_id", "family_id"))
  
  # reorder variables for display purposes
  dat_merged <- dat_merged |> 
    mutate(p_date_of_enrollment = as_date(p_ps_date),
           date_of_enrollment = as_date(ps_date)) |> 
    select(site_id, family_id, wecare_id, p_wecare_id, date_of_enrollment, p_date_of_enrollment,
           everything())
    
  return(dat_merged)
}