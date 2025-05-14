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
      ps_hear_more = coalesce_columns(dat_youth_cleaned, "ps[0-9]{2}y_", "hear_more") |> pull(1),
      ps_willing_to_contact = coalesce_columns(dat_youth_cleaned, "ps[0-9]{2}y_", "willing_to_contact") |> pull(1),
      ps_decline_reason = coalesce_columns(dat_youth_cleaned, "ps[0-9]{2}y_", "decline_reason") |> pull(1),
      ps_youth_name = coalesce_columns(dat_youth_cleaned, "ps[0-9]{2}y_", "youth_name") |> pull(1),
      ps_signature = coalesce_columns(dat_youth_cleaned, "ps[0-9]{2}y_", "signature") |> pull(1),
      ps_date = coalesce_columns(dat_youth_cleaned, "ps[0-9]{2}y_", "date") |> pull(1),
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
  dat_caregiver_cleaned <- dat_caregiver_cleaned |> 
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
      p_over_18   = over_18,
      p_over_12   = over_12
    ) |> 
    as_tibble()
  
  return(dat_caregiver_cleaned)
}

#’ Clean caregiver follow-up data (3- & 6-month)
#’
#’ Adds every baseline-only field as NA so that
#’ clean_caregiver()’s coalesce() and rename() calls never error.
clean_caregiver_followup <- function(dat_caregiver_raw) {
  dat_caregiver_raw %>%
    mutate(
      # language coalescing
      p_screen_language_harlem    = NA_character_,
      p_screen_language_kings     = NA_character_,
      # positioning helpers
      initial_questions_complete  = NA,
      eligibility_screen_complete = NA,
      wecare_id                   = NA,
      p_sw_pause_id               = NA,
      over_18                     = NA,
      over_12                     = NA, 
      p_first_name                = NA_character_,
      # ICF-rename source cols
      harlem_icf18_name_2         = NA_character_,
      harlem_icf18_name_3         = NA_character_,
      kings_icf18_name_2          = NA_character_,
      kings_icf18_name_3          = NA_character_,
      harlem_icf18_name_2_spa     = NA_character_,
      harlem_icf18_name_3_spa     = NA_character_,
      kings_icf18_name_2_spa      = NA_character_,
      kings_icf18_name_3_spa      = NA_character_,
      harlem_icf18_name_2_fre     = NA_character_,
      harlem_icf18_name_3_fre     = NA_character_,
      kings_icf18_name_2_hai      = NA_character_,
      kings_icf18_name_3_hai      = NA_character_
    ) %>%
    clean_caregiver()
}

# Clean 3-month youth follow-up
clean_youth_3m_followup <- function(dat_youth_raw) {
  library(dplyr)
  library(janitor)
  
  df <- dat_youth_raw %>% clean_names()
  
  # 1) Baseline: only keep participant_id
  baseline <- df %>%
    filter(redcap_event_name == "baseline_visit_arm_1") %>%
    select(participant_id) %>%
    distinct()
  
  # 2) 3-month follow-up
  followup_3m <- df %>%
    filter(redcap_event_name == "3_month_followup_arm_1") %>%
    filter(follow_visitstatus_3m == 1)
  
  # 3) Merge on participant_id only
  merged <- inner_join(baseline, followup_3m, by = "participant_id") %>%
    select(participant_id, matches("_3m$|_3m_"))
  
  return(merged)
}

# Clean 6-month youth follow-up
clean_youth_6m_followup <- function(dat_youth_raw) {
  library(dplyr)
  library(janitor)
  
  df <- dat_youth_raw %>% clean_names()
  
  # 1) Baseline: only keep participant_id
  baseline <- df %>%
    filter(redcap_event_name == "baseline_visit_arm_1") %>%
    select(participant_id) %>%
    distinct()
  
  # 2) 6-month follow-up
  followup_6m <- df %>%
    filter(redcap_event_name == "6_month_followup_arm_1") %>%
    filter(follow_visitstatus_6m == 1)
  
  # 3) Merge on participant_id only
  merged <- inner_join(baseline, followup_6m, by = "participant_id") %>%
    select(participant_id, matches("_6m$|_6m_"))
  
  return(merged)
}

#’ Merge youth baseline + 3m + 6m into one wide table with presence flags
#’
#’ @param dat_base   cleaned baseline youth data (must have participant_id)
#’ @param dat_3m     cleaned 3-month follow-up youth data (must have participant_id)
#’ @param dat_6m     cleaned 6-month follow-up youth data (must have participant_id)
#’ @return one data.frame with all columns side-by-side, key = participant_id,
#’   plus i_youth_baseline, i_youth_3m, i_youth_6m (0/1)
merge_youth_all_events <- function(dat_base, dat_3m, dat_6m) {
  library(dplyr)
  
  # Add presence flags
  base_tag  <- dat_base %>% mutate(i_youth_baseline = 1L)
  three_tag <- dat_3m    %>% mutate(i_youth_3m       = 1L)
  six_tag   <- dat_6m    %>% mutate(i_youth_6m       = 1L)
  
  merged <- full_join(base_tag, three_tag, by = "participant_id") %>%
    full_join(six_tag, by = "participant_id") %>%
    mutate(
      i_youth_baseline = replace_na(i_youth_baseline, 0L),
      i_youth_3m       = replace_na(i_youth_3m, 0L),
      i_youth_6m       = replace_na(i_youth_6m, 0L)
    )
  return(merged)
}


#’ Merge caregiver baseline + 3m + 6m into one wide table with presence flags
#’
#’ @param dat_base   cleaned baseline caregiver data (must have p_wecare_id)
#’ @param dat_3m     cleaned 3-month follow-up caregiver data (must have caregiver_id_3m)
#’ @param dat_6m     cleaned 6-month follow-up caregiver data (must have caregiver_id_3m)
#’ @return one data.frame with all columns side-by-side, key = p_wecare_id,
#’   plus i_caregiver_baseline, i_caregiver_3m, i_caregiver_6m (0/1)
merge_caregiver_all_events <- function(dat_base, dat_3m, dat_6m) {
  library(dplyr)
  
  dedupe_by_fewest_nas <- function(df, key) {
    df %>%
      mutate(na_count = rowSums(is.na(select(., -all_of(key))))) %>%
      group_by(across(all_of(key))) %>%
      slice_min(na_count, with_ties = FALSE) %>%
      ungroup() %>%
      select(-na_count)
  }
  
  # Baseline (keep all baseline variables)
  base_tag <- dat_base %>%
    mutate(p_wecare_id = as.character(p_wecare_id),
           i_caregiver_baseline = 1L) %>%
    dedupe_by_fewest_nas("p_wecare_id")
  
  # 3-month (keep only _3m columns)
  three_tag <- dat_3m %>%
    mutate(caregiver_id_3m = as.character(caregiver_id_3m),
           i_caregiver_3m  = 1L) %>%
    select(-any_of("p_wecare_id")) %>%
    rename(p_wecare_id = caregiver_id_3m) %>%
    dedupe_by_fewest_nas("p_wecare_id") %>%
    select(p_wecare_id, i_caregiver_3m, matches("_3m$|_3m_"))
  
  # 6-month (keep only _6m columns)
  six_tag <- dat_6m %>%
    mutate(caregiver_id_3m = as.character(caregiver_id_3m),
           i_caregiver_6m  = 1L) %>%
    select(-any_of("p_wecare_id")) %>%
    rename(p_wecare_id = caregiver_id_3m) %>%
    dedupe_by_fewest_nas("p_wecare_id") %>%
    select(p_wecare_id, i_caregiver_6m, matches("_6m$|_6m_"))
  
  # Merge: baseline + selected follow-up columns
  merged <- base_tag %>%
    full_join(three_tag, by = "p_wecare_id") %>%
    full_join(six_tag,   by = "p_wecare_id") %>%
    mutate(
      i_caregiver_baseline = replace_na(i_caregiver_baseline, 0L),
      i_caregiver_3m       = replace_na(i_caregiver_3m, 0L),
      i_caregiver_6m       = replace_na(i_caregiver_6m, 0L)
    )
  
  return(merged)
}

# Function to merge youth and caregiver data
merge_caregiver_youth <- function(dat_caregiver_cleaned, dat_youth_cleaned){
  
  # merge data (no caregiver data unless youth data is available)
  dat_merged <- dat_caregiver_cleaned |> 
    full_join(dat_youth_cleaned, by = c("site_id", "family_id"))
  
  # reorder variables for display purposes
  dat_merged <- dat_merged |> 
    mutate(p_date_of_enrollment = as_date(p_ps_date),
           date_of_enrollment = as_date(ps_date)) |> 
    select(site_id, family_id, wecare_id, p_wecare_id, date_of_enrollment, p_date_of_enrollment,
           everything())
  
  return(dat_merged)
}

# Function to process merged data for reporting purposes
process_report_data <- function(dat_merged){
  
  dat_report <- dat_merged |> 
    # Create a new column to categorize age group
    mutate(age_group = case_when(
      p_over_18 == 0 | over_18 == 0 ~ "12-17",
      p_over_18 == 1 | over_18 == 1 ~ "18+",
      TRUE ~ NA_character_
    )) |> 
    rowwise() |> 
    mutate(screen_race_eligible = if_any(c(screen_race_1, screen_race_2, screen_race_3, screen_race_4), 
                                         ~ .x == 1)) |> 
    mutate(completed_survey = if_else(
      any(!is.na(c_across(starts_with(c("giso_","cssrs_[0-9]", "st_", "ets_",
                                        "acfs_", "barrier_", "atphs_", "soc_",
                                        "tam_", "pfcs_", "pvp_", "cde_",
                                        "hopeless_", "uppss_", "yrbsa_", "dus_",
                                        "se_", "pet_", "pil_", "cdef_",
                                        "ibelong_", "sc_", "joy_")) &
                            ends_with("_b")
      ))),
      1, 0
    )) |> 
    ungroup() |>
    mutate(site_id = if_else(site_id == "H", "Harlem", "Kings County")) |> 
    mutate(treatment = if_else(treatment == 1, "Treatment", "Control")) |> 
    mutate(
      meet_minimal_risk = if_else(
        eligible_visited_ed == 1 | 
          eligible_hospitalized == 1 | 
          eligible_taken_med == 1 | 
          eligible_received_therapy == 1 | 
          eligible_sought_counseling == 1 | 
          (eligible_anxiety_1 + eligible_anxiety_2 >= 4) | 
          (eligible_depress_1 + eligible_depress_2 >= 4) | 
          (eligible_sch_connect_1 < 3 | eligible_sch_connect_2 < 3) | 
          eligible_self_harm > 0 | 
          eligible_sleep_problem_1 > 1,
        1, 0)
    )
  
  # Manual fixes due to redcap entry error
  dat_report <- dat_report |> 
    mutate(p_informed_consent_form_parent_complete = 
             if_else(!is.na(wecare_id) & p_wecare_id == "K-F0005-C-S", 
                     0, p_informed_consent_form_parent_complete)) |> 
    mutate(age_group = 
             if_else(!is.na(wecare_id) & wecare_id == "K-F0006-Y-S",
                     "18+", age_group)) |> 
    mutate(contact_form_youth_complete = 
             if_else(!is.na(wecare_id) & wecare_id == "K-F0006-Y-S",
                     2, contact_form_youth_complete)) |> 
    mutate(ps_hear_more = 
             if_else(!is.na(wecare_id) & wecare_id == "K-F0006-Y-S",
                     1, ps_hear_more)) |> 
    mutate(ps_willing_to_contact = 
             if_else(!is.na(wecare_id) & wecare_id == "K-F0006-Y-S",
                     1, ps_willing_to_contact)) |> 
    mutate(ps_signature = 
             if_else(!is.na(wecare_id) & wecare_id == "K-F0006-Y-S",
                     "signature_unavailable", ps_signature)) |>
    
    # ✅ New fix: manually set site_id for H-F0167-C-S
    mutate(site_id = if_else(!is.na(p_wecare_id) & p_wecare_id == "H-F0167-C-S", 
                             "Harlem", site_id))
  
  return(dat_report)
}