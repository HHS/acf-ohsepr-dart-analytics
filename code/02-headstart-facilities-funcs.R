#  Head Start locations functions 
#-------------------------------------------------------------------------------
# Purpose: This file stores all of the Head Start script's relevant functions.
#-------------------------------------------------------------------------------

#### expand_abbreviations: function to expand all street names out to achieve 
# address line standardization. 

abbreviations <- c(
  "St" = "Street",
  "Blvd" = "Boulevard",
  "Rd" = "Road",
  "Ave" = "Avenue",
  "Dr" = "Drive",
  "Ln" = "Lane",
  "Ct" = "Court",
  "Pl" = "Place",
  "Sq" = "Square",
  "Pkwy" = "Parkway",
  "Cir" = "Circle",
  "Ter" = "Terrace",
  "Hwy" = "Highway",
  "Way" = "Way",
  "Trl" = "Trail",
  "Plz" = "Plaza",
  "Ctr" = "Center",
  "Expwy" = "Expressway"
)
expand_abbreviations <- function(df, address_column) {
  # Iterate over each abbreviation and its long form
  for (abbr in names(abbreviations)) {
    long_form <- abbreviations[abbr]
    # Replace abbreviations with their long forms
    df[[address_column]] <- str_replace_all(df[[address_column]], 
                                            paste0("\\b", abbr, "\\b"), 
                                            long_form)
  }
  return(df)
}


#### head start to lower: function to lower a subset of character variables
head_start_to_lower <- function(df = raw_hs_df) {
  
  char_cols <- c('service_location_name', 'address_line_one', 
                 'address_line_two', 'city', 'county', 'program_admin_name',
                 'program_admin_address_line_one', 'program_admin_address_line_two',
                 'program_admin_city', 'program_admin_county'
  )
  
  df <- df %>% 
    mutate(across(all_of(char_cols), str_to_lower))
  return(df)
  
}

#### create_facility_hash: function to create the facility hash variable
create_facility_hash <- function(df = expanded_hs_df) {
  
  # Create facility_id variable 
  df <- df %>% 
    mutate(facility_id = paste(address_line_one, zip, state, 
                               sep = "_"))
  # Create facility_hash
  hash_df <- df %>% 
    mutate(facility_hash = sapply(facility_id, digest))
  
  return(hash_df)
  
}

#### write_head_start_locations: function to output cleaned up Head Start 
# facilities location data pulled from the publicly available URL
write_head_start_locations <- function(df = cleaned_headstart) {
  full_file_name <- paste0(HS_dir, 'head_start_locations_', CURR_DATE, '.feather')
  cat(full_file_name, '\n')
  write_feather(x = df, full_file_name)
  cat("The Head Start facilities locations output successfully.")
}