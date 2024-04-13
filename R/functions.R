# coalesce columns
coalesce_columns <- function(data, regex = "ps[0-9]{2}_", keyword) {
  # Find the columns that match the regex pattern combined with the keyword
  cols <- grep(paste0(regex, keyword), names(data), value = TRUE)
  
  data |> 
    # Apply operations row-wise
    rowwise() |> 
    # Create a new or transform an existing column with coalescing logic
    transmute(!!keyword := {
      # Extract values for the current row across selected columns
      values <- c_across(all_of(cols))
      # Remove NA values
      values <- na.omit(values)
      if (length(values) == 0) {
        NA  # Return NA if all values are NA
      } else if (any(values != 0)) {
        values[values != 0][1]  # Return the first non-zero value
      } else {
        0  # Return 0 if all values are zero
      }
    }) |> 
    ungroup()  # Ensure the data is not grouped for subsequent operations
}

