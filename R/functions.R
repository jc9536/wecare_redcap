# coalesce 
coalesce_columns <- function(data, # dataset
                             regex = "ps[0-9]{2}_", # repeated pattern in variable name
                             keyword) { # keyword to identify the variable
  cols <- grep(paste0(regex, keyword), names(data), value = TRUE)
  data %>%
    transmute(!!keyword := coalesce(!!!rlang::syms(cols)))
}