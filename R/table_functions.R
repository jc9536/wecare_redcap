# library(tidyverse)
# library(flextable)
# source(here::here("R/utils.R"))

# Functions to generate tables and figures

# -------- Table 1: Summary of Enrollment --------

tab_enrollment <- function(dat){

  t_enrollment <- dat |>
    # Ensure grouping is done by both site_id and the new age_group column
    group_by(site_id, age_group) |>
    # Use summarise() to compute summaries specific to each group
    summarize(
      n_approached = length(unique(family_id)), 
      n_completed_contact_form = sum((age_group == "12-17" & p_contact_form_parent_complete == 2) | 
                                       (age_group == "18+" & contact_form_youth_complete == 2), na.rm = TRUE),
      n_passed_contact_form = sum((age_group == "12-17" & p_ps_hear_more == 1 & p_ps_willing_to_contact == 1) |
                                    (age_group == "18+" & ps_hear_more == 1 & ps_willing_to_contact == 1), na.rm = TRUE),
      n_completed_eligibility_screen = sum(eligibility_screen_complete == 2, na.rm = TRUE),
      n_completed_eligibility_survey = sum(eligibility_survey_complete == 2, na.rm = TRUE),
      # the first field in eligiblity survey is required, so if eligible_visited_ed is not NA, 
      # it means the person passed the eligiblity screen 
      n_passed_eligibility_screen = sum(is.na(eligible_visited_ed) == F, na.rm = TRUE),
      n_passed_eligibility_survey = sum(eligibility_survey_complete == 2, na.rm = TRUE),
      n_consented = sum((age_group == "12-17" & p_informed_consent_form_parent_complete == 2) | 
                          (age_group == "18+" & informed_consent_form_youth_complete == 2), na.rm = TRUE),
      .groups = 'drop'  # Drop the grouping once summarization is done
    ) |> 
    filter(!is.na(age_group)) |> 
    # Ensure all combinations of site_id and age_group appear in the results
    complete(site_id = c("Harlem", "Kings County"), age_group = c("12-17", "18+"), fill = list(
      n_approached = 0,
      n_completed_contact_form = 0, 
      n_passed_contact_form = 0,
      n_completed_eligibility_screen = 0,
      n_passed_eligibility_screen = 0,
      n_completed_eligibility_survey = 0,
      n_passed_eligibility_survey = 0,
      n_consented = 0
    )) |> 
    # Create percentage variables
    mutate(
      perc_consented_contact = if_else(n_completed_contact_form > 0, n_consented / n_completed_contact_form, NA_real_),
      perc_consented_eligible = if_else(n_passed_eligibility_survey > 0, n_consented / n_passed_eligibility_survey, NA_real_)
    ) |> 
    mutate(across(starts_with("perc"), ~ scales::percent(.x, accuracy = 1)))
  
  # Rename
  names(t_enrollment) <- c("", "Age Group",
                 "Number of families approached, IDs assigned",
                 "Number of families approached, SW completed contact form",
                 "Number of families who passed the screen in the contact form",
                 "Number of youth who completed the eligibility screen",
                 "Number of youth who passed the eligibility screen",
                 "Number of youth who completed the eligibility survey",
                 "Number of youth who passed the eligibility survey",
                 "Number of families who consented",
                 "Percent of families who consented among those who completed the contact form",
                 "Percent of youth who consented among those who passed the eligibility survey")
  
  # Transpose
  t_enrollment <- t_enrollment |> 
    t() |> 
    as.data.frame() |> 
    rownames_to_column()
  
  # Create flextable
  ft_enrollment <- flextable(t_enrollment) |>
    width(width = 1.5) |>
    width(j = 1, width = 4) |>
    delete_part(part = "header") |>
    hline_top(j = 1:ncol(t_enrollment) , part = "body") |>
    hline(i = 1:2 , part = "body") |> 
    align(j = 2:5, align = "center", part = "body") |> 
    bold(i = 1:2, bold = TRUE, part = "body") |> 
    merge_h(i = 1:2)
  
  return(ft_enrollment)
}

# -------- Table 2: Number of participants enrolled during each one-week recruitment period --------

tab_weekly_enrollment <- function(dat){

  t_weekly_enrollment <- dat |>
    mutate(date_of_enrollment_all = coalesce(p_date_of_enrollment, date_of_enrollment, screen_doe)) |>
    mutate(date_of_enrollment_range = get_week_range(date_of_enrollment_all)) |> 
    filter(date_of_enrollment_range != "NA ~ NA")
  
  t_weekly_enrollment <- t_weekly_enrollment |> 
    # Ensure grouping is done by both site_id and the new age_group column
    group_by(site_id, age_group, date_of_enrollment_range) |>
    # Use summarise() to compute summaries specific to each group
    summarize(
      n_consented = sum((age_group == "12-17" & p_informed_consent_form_parent_complete == 2) | 
                          (age_group == "18+" & informed_consent_form_youth_complete == 2), na.rm = TRUE),
      .groups = 'drop'  # Drop the grouping once summarization is done
    ) |> 
    # Ensure all combinations of site_id and age_group appear in the results
    complete(site_id = c("Harlem", "Kings County"), 
             age_group = c("12-17", "18+"), 
             date_of_enrollment_range = na.omit(unique(t_weekly_enrollment$date_of_enrollment_range)),
             fill = list(
      n_consented = 0
    ))
  
  # Rename
  names(t_weekly_enrollment) <- c("Site", "Age Group",
                                  "Date Range of Enrollment",
                                  "Number of youth who enrolled")
  
  t_weekly_enrollment <- t_weekly_enrollment |> 
    pivot_wider(names_from = `Date Range of Enrollment`, values_from = `Number of youth who enrolled`)
  
  
  # Transpose
  t_weekly_enrollment <- t_weekly_enrollment |> 
    t() |> 
    as.data.frame() |> 
    rownames_to_column() |> 
    mutate(week = if_else(rowname %in% c("Site", "Age Group"),
                          NA,
                          rowname
                          ),
           .before = rowname) |> 
    mutate(week = calculate_week_number(week))
  
  t_weekly_enrollment$rowname[1] <- ""
  t_weekly_enrollment$week[2] <- "Week"
  t_weekly_enrollment$rowname[2] <- "Date range of report"
  
  # Create flextable
  ft_weekly_enrollment <- flextable(t_weekly_enrollment) |>
    width(width = 1.5) |>
    width(j = 2, width = 2) |>
    delete_part(part = "header") |>
    hline_top(j = 1:ncol(t_weekly_enrollment) , part = "body") |>
    hline(i = 1:2, part = "body") |> 
    align(j = 1:6, align = "center", part = "body") |> 
    bold(i = 1:2, bold = TRUE, part = "body") |> 
    merge_h(i = 1)
  
  return(ft_weekly_enrollment)
}

# -------- Table 3: Detailed Recruitment Information --------

tab_recruitment <- function(dat){

  t_recruitment <- dat |>
    # Ensure grouping is done by both site_id and the new age_group column
    group_by(site_id) |>
    # Use summarise() to compute summaries specific to each group
    summarize(
      n_completed_contact_form = sum((age_group == "12-17" & p_contact_form_parent_complete == 2) | 
                                       (age_group == "18+" & contact_form_youth_complete == 2), na.rm = TRUE),
      
      n_passed_contact_form = sum((age_group == "12-17" & p_ps_hear_more == 1 & p_ps_willing_to_contact == 1) |
                                    (age_group == "18+" & ps_hear_more == 1 & ps_willing_to_contact == 1), na.rm = TRUE),
      # the first field in eligiblity survey is required, so if eligible_visited_ed is not NA, 
      # it means the person passed the eligiblity screen 
      n_passed_eligibility_screen = sum(is.na(eligible_visited_ed) == F, na.rm = TRUE),
      n_passed_eligibility_survey = sum(meet_minimal_risk == 1, na.rm = TRUE),
      n_consented = sum((age_group == "12-17" & p_informed_consent_form_parent_complete == 2) | 
                          (age_group == "18+" & informed_consent_form_youth_complete == 2), na.rm = TRUE),
  
      # n_not_completed_contact_form = sum(!((age_group == "12-17" & p_contact_form_parent_complete == 2) | 
      #                                        (age_group == "18+" & contact_form_youth_complete == 2)), na.rm = TRUE),
      # n_not_passed_eligibility_screen = sum(!(is.na(eligible_visited_ed)) == F, na.rm = TRUE),
      # n_not_passed_eligibility_survey = sum(!(meet_minimal_risk == 1), na.rm = TRUE),
      # n_not_consented = sum((age_group == "12-17" & p_informed_consent_form_parent_complete != 2) & 
      #                           (age_group == "18+" & informed_consent_form_youth_complete != 2), na.rm = TRUE),
      
      n_not_hear_more = sum(!((age_group == "12-17" & p_ps_hear_more == 1) | 
                                (age_group == "18+" & ps_hear_more == 1)), na.rm = TRUE),
      n_not_willing_to_contact = sum(!((age_group == "12-17" & p_ps_willing_to_contact == 1) | 
                                         (age_group == "18+" & ps_willing_to_contact == 1)), na.rm = TRUE),
      n_not_p_ps_signature = sum(age_group == "12-17" & is.na(p_ps_signature), na.rm = T),
      n_not_ps_signature = sum(age_group == "18+" & is.na(ps_signature), na.rm = T),
      
      n_not_screen_age = sum(!(screen_age >= 12 & screen_age <= 19), na.rm = T),
      n_not_screen_caregiver_present = sum(screen_age >= 12 & screen_age <= 17 & screen_caregiver_present == 0, na.rm = T),
      n_not_screen_race_eligible = sum(!(screen_race_eligible == 1), na.rm = T),
      n_not_screen_has_cell_phone = sum(!(screen_has_cell_phone == 1), na.rm = T),
      n_not_screen_speak_read_english = sum(!(screen_speak_read_english == 1), na.rm = T),
      n_seek_therapy = sum(screen_in_treatment == 1 & screen_seek_therapy == 1, na.rm = T),
      n_screen_self_repeat = sum(screen_self_repeat == 1, na.rm = T),
      n_screen_sibling_repeat = sum(screen_sibling_repeat == 2, na.rm = T),
      n_not_meet_minimal_risk = sum(!meet_minimal_risk, na.rm = T),
      n_total = length(unique(family_id)),
      .groups = 'drop'  # Drop the grouping once summarization is done
    ) |> 
    # Ensure all combinations of site_id and age_group appear in the results
    complete(site_id = c("Harlem", "Kings County"), fill = list(
      n_completed_contact_form = 0, 
      n_passed_contact_form = 0,
      n_passed_eligibility_screen = 0,
      n_passed_eligibility_survey = 0,
      n_consented = 0,
      # n_not_completed_contact_form = 0, 
      # n_not_passed_eligibility_screen = 0,
      # n_not_passed_eligibility_survey = 0,
      # n_not_consented = 0,
      n_not_hear_more = 0,
      n_not_willing_to_contact = 0,
      n_not_p_ps_signature = 0,
      n_not_ps_signature = 0,
      n_not_screen_age = 0,
      n_not_screen_caregiver_present = 0,
      n_not_screen_race_eligible = 0,
      n_not_screen_race_eligible = 0,
      n_not_screen_has_cell_phone = 0,
      n_not_screen_speak_read_english = 0,
      n_seek_therapy = 0,
      n_screen_self_repeat = 0,
      n_screen_sibling_repeat = 0,
      n_not_meet_minimal_risk = 0,
      n_total = 0
    )) 
  
  # Transpose
  t_recruitment <- t_recruitment |> 
    t() |> 
    as.data.frame() |> 
    rownames_to_column()
  
  # Rename
  t_recruitment$rowname <- c(
    "",
    "Number of families approached, SW completed contact form",
    "Number of families who passed the screen in the contact form",
    "Number of youth who passed the eligibility screen",
    "Number of youth who passed the eligibility survey",
    "Number of families who consented",
    
    # "Number of youth who DID NOT complete the contact form",
    # "Number of youth who DID NOT pass the eligibility screen",
    # "Number of youth who DID NOT pass the eligibility survey",
    # "Number of youth who DID NOT consent",
    
    "Number of families who were NOT willing to hear more about the study",
    "Number of families who were NOT willing to be contacted",
    "Number of caregivers (with youth aged 12-17) who DID NOT consent in the contact form",
    "Number of youth aged 18+ who DID NOT consent in the contact form",
    
    "Number of youth who were either below 12 or above 19",
    "Number of youth aged 12-17 whose caregiver was NOT present",
    "Number of youth whose race was NOT Black",
    "Number of youth who DID NOT have a cellphone to receive texts",
    "Number of youth who were NOT able to speak or read in English",
    "Number of youth who has sought therapy in the past week",
    "Number of youth who were currently enrolled in WeCare",
    "Number of youth who had another youth family member in WeCare",
    "Number of youth who DID NOT the minimal risk criteria",
    "Total number of families approached, IDs assigned"
  )
  
  t_recruitment <- t_recruitment |> 
    add_row(rowname = "General Recruitment Information", .after = 1) |> 
    add_row(rowname = "Ineligible Based on the First Screen (Information from the Contact Form)", .after = 7) |> 
    add_row(rowname = "Ineligible Based on the Second Screen (Information from Youth Eligibility Screen and Survey)", .after = 12)
  
  # Create flextable
  ft_recruitment <- flextable(t_recruitment) |> 
    width(width = 1.5) |>
    width(j = 1, width = 6) |>
    delete_part(part = "header") |>
    hline_top(j = 1:ncol(t_recruitment) , part = "body") |>
    hline(i = c(1, 7, 12, 22) , part = "body") |> 
    align(align = "center", part = "header") |> 
    align(j = 2:3, align = "center", part = "body") |> 
    bold(i = c(1, 2, 8, 13), bold = TRUE, part = "body") |> 
    italic(i = 22, italic = TRUE, part = "body") |> 
    merge_h(i = 1)
  
  return(ft_recruitment)
}

# -------- Table 4: Treatment/Control Group Assignment --------

tab_assignment <- function(dat){
  
  t_assignment <- dat |>
    # Ensure grouping is done by both site_id and the new age_group column
    group_by(site_id, age_group, treatment) |>
    # Use summarise() to compute summaries specific to each group
    summarize(
      n_consented = sum((age_group == "12-17" & p_informed_consent_form_parent_complete == 2) | 
                          (age_group == "18+" & informed_consent_form_youth_complete == 2), na.rm = TRUE),
      n_completed_survey = sum(completed_survey, na.rm = T),
      n_completed_cassy = sum(cassy_results_complete == 2, na.rm = T),
      n_cassy_positive = sum(cassy_result == 1, na.rm = T),
      n_received_cfs = sum(swdf_received_cfs == 1, na.rm = T),
      .groups = 'drop'  # Drop the grouping once summarization is done
    ) |> 
    # Ensure all combinations of site_id and age_group appear in the results
    complete(site_id = c("Harlem", "Kings County"), 
             treatment = c("Treatment", "Control"),
             age_group = c("12-17", "18+"), fill = list(
               n_consented = 0,
               n_completed_survey = 0,
               n_completed_cassy = 0,
               n_cassy_positive = NA,
               n_received_cfs = NA
             )) |> 
    filter(is.na(treatment) == F)
  
  # Rename
  names(t_assignment) <- c("", "Treatment Condition","Age Group",
                           "Number of youth who consented",
                           "Number of youth who completed baseline survey",
                           "Number of youth who completed CASSY",
                           "Number of youth who screened positive (> 0.05) on CASSY",
                           "Number of youth who received Connections for Safety"
  )
  
  # Transpose
  t_assignment <- t_assignment |> 
    t() |> 
    as.data.frame() |> 
    rownames_to_column()
  
  # Create flextable
  ft_assignment <- flextable(t_assignment) |> 
    width(width = 1.1) |>
    width(j = 1, width = 1.5) |>
    delete_part(part = "header") |>
    hline_top(j = 1:ncol(t_assignment) , part = "body") |>
    hline(i = 1, part = "body") |> 
    hline(i = 3, part = "body") |>
    align(align = "center", part = "header") |> 
    align(align = "center", part = "body") |> 
    bold(i = 1:3, bold = TRUE, part = "body") |> 
    merge_h(i = 1:2)
  
  return(ft_assignment)
  
}

# -------- Table 5: Triggered Risk Management Protocol During Baseline --------

tab_trigger_rmp <- function(dat){
  
  t_trigger_rmp <- dat |>
    rowwise() |>
    mutate(
      cssrs_positive = if_else(
        any(c_across(c(cssrs_3b_b, cssrs_4b_b, cssrs_5b_b, cssrs_6b_b, cssrs_7b_b)) == 1),
        1, 
        0
      )
    ) |>
    ungroup() |>  
    group_by(site_id) |> 
    summarize(
      n_disclosure_suicide = sum(disclose_suicide, na.rm = T),
      n_cssrs_positive = sum(cssrs_positive, na.rm = T),
      n_cassy_positive = sum(cassy_result, na.rm = T)
    ) |> 
    # Ensure all combinations of site_id and age_group appear in the results
    complete(site_id = c("Harlem", "Kings County"), fill = list(
      n_disclosure_suicide = NA,
      n_cssrs_positive = NA,
      n_cassy_positive = NA
    ))
  
  # Rename
  names(t_trigger_rmp) <- c("", 
                            "Number of youth who verbally disclosed suicide ideation",
                            "Number of youth who screened positive on CSSRS",
                            "Number of youth who screened positive on CASSY"
  )
  
  # Transpose
  t_trigger_rmp <- t_trigger_rmp |> 
    t() |> 
    as.data.frame() |> 
    rownames_to_column()
  
  # Create flextable
  ft_trigger_rmp <- flextable(t_trigger_rmp) |> 
    width(width = 1.5) |>
    width(j = 1, width = 4) |>
    delete_part(part = "header") |>
    hline_top(j = 1:ncol(t_trigger_rmp) , part = "body") |>
    hline(i = 1 , part = "body") |> 
    align(align = "center", part = "header") |> 
    align(align = "center", part = "body") |> 
    bold(i = 1, bold = TRUE, part = "body")
  
  return(ft_trigger_rmp)
  
}

# -------- Table 6: Demographics of Enrolled Subjects (DSMB Chart) --------

tab_demo <- function(dat){
  
  t_demo <- dat |> 
    group_by(site_id, treatment) |>
    summarize(
      # age
      age_12_17 = sum(age_group == "12-17", na.rm = T),
      age_18_over = sum(age_group == "18+", na.rm = T),
      
      # sex at birht
      sex_male = sum(screen_sex == 1, na.rm = T),
      sex_female = sum(screen_sex == 2, na.rm = T),
      
      # gender identity
      gender_male = sum(giso_gender_identity_b == 1, na.rm = T),
      gender_female = sum(giso_gender_identity_b == 2, na.rm = T),
      gender_nonbinary = sum(giso_gender_identity_b == 3, na.rm = T),
      gender_genderfluid = sum(giso_gender_identity_b == 4, na.rm = T),
      gender_genderqueer = sum(giso_gender_identity_b == 5, na.rm = T),
      gender_notsure = sum(giso_gender_identity_b == 66, na.rm = T),
      gender_notknow = sum(giso_gender_identity_b == 77, na.rm = T),
      gender_notidentify = sum(giso_gender_identity_b == 88, na.rm = T),
      gender_noanswer = sum(giso_gender_identity_b == 99, na.rm = T),
      
      # race
      race_black = sum(screen_race_1 == 1, na.rm = T),
      race_africanamerican = sum(screen_race_2 == 1, na.rm = T),
      race_african = sum(screen_race_3 == 1, na.rm = T),
      race_caribbeanwestindian = sum(screen_race_4 == 1, na.rm = T),
      race_native = sum(screen_race_5 == 1, na.rm = T),
      race_asian = sum(screen_race_6 == 1, na.rm = T),
      race_pacificislander = sum(screen_race_7 == 1, na.rm = T),
      race_white = sum(screen_race_8 == 1, na.rm = T),
      race_unknown = sum(screen_race_99 == 1, na.rm = T),
      
      # ethnicity
      ethnicity_latino = sum(screen_ethnicity == 1, na.rm = T),
      ethnicity_notlatino = sum(screen_ethnicity == 2, na.rm = T),
      ethnicity_unknown = sum(screen_ethnicity == 99, na.rm = T),
      
      .groups = 'drop'  # Drop the grouping once summarization is done
    ) |> 
    # Ensure all combinations of site_id and age_group appear in the results
    complete(site_id = c("Harlem", "Kings County"), treatment = c("Treatment", "Control"), fill = list(
      age_12_17 = NA,
      age_18_over = NA,
      sex_male = NA,
      sex_female = NA,
      sex_male = NA,
      sex_female = NA,
      gender_male = NA,
      gender_female = NA,
      gender_nonbinary = NA,
      gender_genderfluid = NA,
      gender_genderqueer = NA,
      gender_notsure = NA,
      gender_notknow = NA,
      gender_notidentify = NA, 
      gender_noanswer = NA,
      race_black = NA,
      race_africanamerican = NA,
      race_african = NA,
      race_caribbeanwestindian = NA,
      race_native = NA,
      race_asian = NA,
      race_pacificislander = NA,
      race_white = NA,
      race_unknown = NA,
      ethnicity_latino = NA,
      ethnicity_notlatino = NA,
      ethnicity_unknown = NA
    )) |> 
    filter(is.na(treatment) != T)
  
  # add sum variables
  t_demo <- t_demo |> 
    rowwise() |> 
    mutate(age_total = sum(c_across(starts_with("age"))),
           .after = age_18_over) |> 
    mutate(sex_total = sum(c_across(starts_with("sex"))),
           .after = sex_female) |> 
    mutate(gender_total = sum(c_across(starts_with("gender"))),
           .after = gender_noanswer) |> 
    # mutate(race_total = sum(c_across(starts_with("race"))),
    #        .after = race_unknown) |> 
    # the total count should be the same for each section (e.g., for race or gender)
    # the rowsum is not accurate because race is multiple selection
    mutate(race_total = gender_total, .after = race_unknown) |> 
    mutate(ethnicity_total = sum(c_across(starts_with("ethnicity"))),
           .after = ethnicity_unknown) |> 
    ungroup()
  
  # Apply the transformation to each group
  t_demo <- t_demo |>
    transform_columns("age", "age_total") |>
    transform_columns("sex", "sex_total") |>
    transform_columns("gender", "gender_total") |>
    transform_columns("race", "race_total") |>
    transform_columns("ethnicity", "ethnicity_total")
  
  # Transpose
  t_demo <- t_demo |> 
    t() |> 
    as.data.frame() |> 
    rownames_to_column()
  
  # Rename
  t_demo$rowname <- c(
    "", "Treatment",
    "12-17", "18-19", "Total",
    "Male", "Female", "Total",
    "Male", "Female", "Nonbinary", "Genderfluid", "Genderqueer",
    "I am not sure or questioning", "I don't know what this question means",
    "I do not identify as any of these options", "I do not want to answer", 
    "Total",
    "Black",
    "Black (African American)",
    "Black (African)",
    "Black (Caribbean/West Indian)",
    "American Indian/Alaskan Native",
    "Native Hawaiian Or Other Pacific Islander",
    "Asian",
    "White",
    "Unknown or not reported",
    "Total", 
    "Hispanic origin",
    "Non-Hispanic Origin",
    "Unknown or not reported",
    "Total"
  )
  
  t_demo <- t_demo |> 
    add_row(rowname = "Age", .after = 2) |> 
    add_row(rowname = "Sex assigned at birth", .after = 6) |> 
    add_row(rowname = "Gender", .after = 10) |> 
    add_row(rowname = "Race (multiple selection)", .after = 21) |> 
    add_row(rowname = "Ethnicity", .after = 32)
  
  # Create flextable
  ft_demo <- flextable(t_demo) |> 
    width(width = 1.5) |>
    width(j = 1, width = 3) |>
    delete_part(part = "header") |>
    hline_top(j = 1:ncol(t_demo) , part = "body") |>
    hline(i = c(1:2, 6, 10, 21, 32) , part = "body") |> 
    align(align = "center", part = "header") |> 
    align(j = 2:5, align = "center", part = "body") |> 
    bold(i = c(1:2, 3, 7, 11, 22, 33), bold = TRUE, part = "body") |> 
    italic(i = c(6, 10, 21, 32), italic = TRUE, part = "body") |> 
    merge_h(i = 1)
  
  return(ft_demo)
  
}

# -------- Table 7: Enrollment Table: Projected vs. Actual by Time (cumulative over time) --------

tab_monthly_enrollment_preprocess <- function(dat){
  
  t_monthly_enrollment_actual <- dat |>
    mutate(date_of_enrollment_all = coalesce(p_date_of_enrollment, date_of_enrollment, screen_doe)) |>
    mutate(month_year_of_enrollment = format(date_of_enrollment_all, "%m/%Y"))
  
  t_monthly_enrollment_actual <- t_monthly_enrollment_actual |> 
    # Ensure grouping is done by both site_id and the new age_group column
    group_by(site_id, month_year_of_enrollment) |>
    # Use summarise() to compute summaries specific to each group
    summarize(
      n_randomized = sum(is.na(treatment) == F, na.rm = TRUE),
      .groups = 'drop'  # Drop the grouping once summarization is done
    ) |> 
    # Ensure all combinations of site_id and age_group appear in the results
    complete(site_id = c("Harlem", "Kings County"), 
             month_year_of_enrollment = na.omit(unique(t_monthly_enrollment_actual$month_year_of_enrollment)),
             fill = list(
               n_randomized = 0
             )) |> 
    pivot_wider(names_from = site_id, values_from = n_randomized) |> 
    mutate(actual_enrollment = sum(c_across(c(Harlem, `Kings County`)), na.rm = T),
           .after = month_year_of_enrollment)
  
  # Create target enrollment table
  
  # Starting value
  initial_value <- 40
  # Create an empty data frame
  t_monthly_enrollment_target <- data.frame(month_year_of_enrollment = as.Date(character()), 
                                            target_enrollment = numeric())
  # Increment value
  increment <- 45
  
  # Current sum and the current increment count
  current_sum <- initial_value
  current_date <- as.Date("2024-04-01")
  
  # Fill the data frame until the sum reaches or exceeds 2200
  while (current_sum <= 2200) {
    t_monthly_enrollment_target <- rbind(t_monthly_enrollment_target, data.frame(month_year_of_enrollment = current_date, target_enrollment = current_sum))
    current_date <- seq(current_date, by = "1 month", length.out = 2)[2]
    current_sum <- current_sum + increment
  }
  
  t_monthly_enrollment_target <- t_monthly_enrollment_target |> 
    mutate(month_year_of_enrollment = format(month_year_of_enrollment, "%m/%Y"))
  
  # Combine target and actual tables
  t_monthly_enrollment <- t_monthly_enrollment_target |> 
    left_join(t_monthly_enrollment_actual, by = "month_year_of_enrollment") |> 
    mutate_all(~ifelse(is.na(.), 0, .))
  
  return(t_monthly_enrollment)
}

tab_monthly_enrollment <- function(t_monthly_enrollment) {
  # Get the column names dynamically
  col_names <- names(t_monthly_enrollment)
  n_cols <- length(col_names)
  
  # Create one row of NA for each YEAR label
  year_labels <- c("YEAR 1 (in grant cycle)",
                   "YEAR 2 (in grant cycle)",
                   "YEAR 3 (in grant cycle)",
                   "YEAR 4 (in grant cycle)",
                   "YEAR 5 (in grant cycle)")
  
  year_rows <- lapply(year_labels, function(label) {
    row <- as.list(rep(NA, n_cols))
    row[[1]] <- label  # Insert label in first column
    row
  }) |> 
    purrr::map_dfr(~ setNames(.x, col_names))  # Combine and name columns
  
  # Insert rows after specified indices
  split_indices <- c(0, 10, 23, 36, 49)
  sections <- list()
  
  for (i in seq_along(split_indices)) {
    start <- split_indices[i] + 1
    end <- if (i == length(split_indices)) nrow(t_monthly_enrollment) else split_indices[i + 1]
    chunk <- t_monthly_enrollment[start:end, , drop = FALSE]
    sections[[length(sections) + 1]] <- year_rows[i, ]
    sections[[length(sections) + 1]] <- chunk
  }
  
  ft_monthly_enrollment <- bind_rows(sections)
  
  # Build flextable
  ft_monthly_enrollment <- flextable(ft_monthly_enrollment) |>
    width(width = 1) |>
    width(j = 1, width = 2) |>
    align(j = 1:n_cols, align = "center", part = "body") |>
    align(j = 1:n_cols, align = "center", part = "header") |>
    bold(i = 1, bold = TRUE, part = "header") |>
    bold(i = seq(2, by = 2, length.out = 5), bold = TRUE, part = "body") |>
    hline(i = seq(3, by = 2, length.out = 4), part = "body")  # Draw lines below each section
  
  return(ft_monthly_enrollment)
}

# -------- Figure 1: Enrollment Graph: Projected vs. Actual by Time (cumulative over time) --------

fig_monthly_enrollment <- function(t_monthly_enrollment){
  
  f_monthly_enrollment <- t_monthly_enrollment |>
    select(month_year_of_enrollment, target_enrollment, actual_enrollment) |> 
    pivot_longer(
      cols = c(target_enrollment, actual_enrollment),
      names_to = "type", 
      values_to = "total")
  
  f_monthly_enrollment <- f_monthly_enrollment |> 
    mutate(month_year_of_enrollment = factor(month_year_of_enrollment, levels = (unique(f_monthly_enrollment$month_year_of_enrollment)))) |> 
    mutate(type = if_else(type == "target_enrollment", "Trial Projected", "Trial Actual"))
  
  
  p_monthly_enrollment <- ggplot(f_monthly_enrollment, 
                                 aes(x = month_year_of_enrollment, 
                                     y = total, 
                                     color = type,
                                     group = type)) +
    geom_point() +
    geom_line() + 
    labs(x = "Month & Year", y = "Number of Youth Enrolled in WeCare",
         color = "") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  return(p_monthly_enrollment)
  
}
