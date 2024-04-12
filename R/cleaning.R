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
      ps_hear_more = coalesce_columns(dat_caregiver_cleaned, "ps[0-9]{2}_", "hear_more")[,1],
      ps_willing_to_contact = coalesce_columns(dat_caregiver_cleaned, "ps[0-9]{2}_", "willing_to_contact")[,1],
      ps_decline_reason = coalesce_columns(dat_caregiver_cleaned, "ps[0-9]{2}_", "decline_reason")[,1],
      ps_caregiver_name = coalesce_columns(dat_caregiver_cleaned, "ps[0-9]{2}_", "caregiver_name")[,1],
      ps_youth_name = coalesce_columns(dat_caregiver_cleaned, "ps[0-9]{2}_", "youth_name")[,1],
      ps_signature = coalesce_columns(dat_caregiver_cleaned, "ps[0-9]{2}_", "signature")[,1],
      ps_date = coalesce_columns(dat_caregiver_cleaned, "ps[0-9]{2}_", "date")[,1],
      .after = initial_questions_complete
    ) |> 
    # remove the original un-coalesced variables
    select(-all_of(
      grep(pattern = paste0("^(?!ps_).*(", 
                            paste(c("hear_more", "willing_to_contact", "decline_reason", 
                                    "caregiver_name", "youth_name", "signature", "date"), 
                                  collapse = "|"), 
                            ")"), 
           names(dat_caregiver_cleaned), value = TRUE, perl = TRUE)
      
    )) |>
    
    # coalesce prescreening contact form completion indicators
    mutate(prescreening_contact_form_parent_complete = coalesce_columns(dat_caregiver_cleaned, ".*prescreening_contact_form.*", "complete"),
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
  
  dat_caregiver_cleaned <- dat_caregiver_cleaned |> 
    mutate(
      icf_name_1 = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "name_1")[,1],
      icf_name_2 = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "name_2")[,1],
      icf_name_3 = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "name_3")[,1],
      icf_name_4 = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "name_4")[,1],
      icf_nih_share = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "nih_share")[,1],
      icf_name_5 = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "name_5")[,1],
      icf_nih_first_name = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "first_name")[,1],
      icf_nih_middle_name = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "middle_name")[,1],
      icf_nih_last_name = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "last_name")[,1],
      icf_nih_dob = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "dob")[,1],
      icf_nih_sex = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "sex")[,1],
      icf_nih_city = coalesce_columns(dat_caregiver_cleaned, "icf[0-9]{2}_", "city")[,1],
      .after = eligibility_screen_complete
    ) |> 
    # remove the original un-coalesced variables
    select(-matches("^(harlem|kings)_icf\\d{2}.*")) |> 
    # coalesce prescreening contact form completion indicators
    mutate(informed_consent_form_parent_complete = coalesce_columns(dat_caregiver_cleaned, ".*subject_information_and_informed.*", "complete"),
           .before = p_sw_pause_id) |> 
    # remove the original un-coalesced variables
    select(-matches("subject_information_and_informed"))
  
  return(dat_caregiver_cleaned)
}

# Function to merge youth and caregiver data