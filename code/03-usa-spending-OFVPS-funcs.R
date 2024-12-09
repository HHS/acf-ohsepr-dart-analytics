#### USA Spending -  Office of Family Violence Prevention and Services Functions 
#-------------------------------------------------------------------------------
# Purpose: To apply our 5 methods of data subsetting to our full_grants_df 
# to more accurately identify OHS grants.
# 5 methods are below:
# 1) ACF Program Codes regex filtering - using ACF program  codes
# 2) `CFDA title` regex filtering - e.g., "Family Violence"
# 3) `program activities` code regex filtering - e.g., "Family Violence"
# 4) `funding_office_name` regex filtering - e.g.,"Family Violence"
# 5) combo method combining 2, 3 *and* 4! 

#-------------------------------------------------------------------------------
#  Office of Family Violence Prevention and Services
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
##### Method 1: get_OFVPS_pc_codes_meth(): function to subset our grants data to
# only grants that contain Head Start program codes as identified in the 
# ACF Program Maps input dataset 
#-------------------------------------------------------------------------------
get_OFVPS_pc_codes_meth <- function(grantee_df = full_grants_df) {
  
  program_map <- acf_program_map %>% 
    filter(program == 'Office of Family Violence Prevention and Response')
  
  OFFICE_NAME <- unique(program_map$program)
  
  # Apply FAIN ID location for Discretionary grant types
  # -----------------------------------------------------
  discretionary_codes <- program_map %>% 
    filter(grant_program_type == 'Discretionary')
  
  program_codes_disc <- discretionary_codes$grant_program %>%
    str_split('-') %>% 
    sapply("[", 1) %>% 
    trimws()
  
  # Construct the regex pattern to match [numeric][alpha][numeric] 
  # Apply only to discretionary programs 
  pc_regex_disc <- paste0('(', paste(program_codes_disc, collapse = "|"), ')')
  complete_regex_disc <- paste0("[0-9]+", pc_regex_disc, "[0-9]+")

  # Apply FAIN ID location for Mandatory grant types  
  # -----------------------------------------------------
  mandatory_codes <- program_map %>% 
    filter(grant_program_type == 'Mandatory')
  program_codes_mand <- mandatory_codes$grant_program %>%
    str_split('-') %>% 
    sapply("[", 1) %>% 
    trimws()
  # browser()
  
  # Construct the regex pattern to match [numeric][alpha][numeric] 
  # Apply only to discretionary programs 
  complete_regex_mand <- paste0(paste0(program_codes_mand, collapse = "$|"), "$")

  # Flag corresponding matches 
  regex_fain_mand <- grepl(complete_regex_mand, grantee_df$award_id_fain, perl = TRUE)
  regex_fain_disc <- grepl(complete_regex_disc, grantee_df$award_id_fain, perl = TRUE)
  
  OFVPS_terms <- c('violence')
  
  # Create our output dataframe 
  out_df <- rbind(grantee_df[regex_fain_mand,], grantee_df[regex_fain_disc,]) %>% 
    unique() %>% 
    mutate(cfda_flag = ifelse(grepl(OFVPS_terms, cfda_title, 
                                    ignore.case = TRUE),1, 0), 
           funding_office_flag =ifelse( grepl(OFVPS_terms, 
                                              funding_office_name, 
                                              ignore.case = TRUE), 1, 0),
           prog_activities_flag = ifelse(grepl(OFVPS_terms, 
                                               program_activities_funding_this_award, 
                                               ignore.case = TRUE), 1, 0)
    ) %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag,
                                                prog_activities_flag)))/3,
           assigned_office = OFFICE_NAME) %>% 
    arrange(assistance_transaction_unique_key)
  
  return(out_df)
  
}

# test_df <- get_OFVPS_pc_codes_meth()
# nrow(test_df)
# table(test_df$cfda_title)
# table(test_df$program_activities_funding_this_award)

# ------------------------------------------------------------------------------
##### Method 2: get_OFVPS_cfda_meth(): function to subset the grants data based 
# off of a regex on "Head Start" CFDA
get_OFVPS_cfda_meth <- function(grantee_df = full_grants_df) {
  
  program_map <- acf_program_map %>% 
    filter(program == 'Office of Family Violence Prevention and Response')
  
  OFFICE_NAME <- unique(program_map$program)
  
  OFVPS_terms <- c('violence')
  OFVPS_cfda <- grantee_df %>% 
    filter(grepl(OFVPS_terms, cfda_title, ignore.case = TRUE)) %>% 
    mutate(cfda_flag = ifelse(grepl(OFVPS_terms, cfda_title, 
                                    ignore.case = TRUE),1, 0), 
           funding_office_flag =ifelse( grepl(OFVPS_terms, 
                                              funding_office_name, 
                                              ignore.case = TRUE), 1, 0),
           prog_activities_flag = ifelse(grepl(OFVPS_terms, 
                                               program_activities_funding_this_award, 
                                               ignore.case = TRUE), 1, 0)
    ) %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag, 
                                                prog_activities_flag)))/3,
           assigned_office = OFFICE_NAME) %>% 
    arrange(assistance_transaction_unique_key)
  
  return(OFVPS_cfda)
  
}

test_df <- get_OFVPS_cfda_meth()
table(test_df$cfda_title)
#-------------------------------------------------------------------------------
##### Method 3: get_OFVPS_prog_acts_meth(): function that applies a 
# regex on "Head Start" program_activities_funding_this_award  
#-------------------------------------------------------------------------------
get_OFVPS_prog_acts_meth <- function(grantee_df = full_grants_df) {
  # browser()
  program_map <- acf_program_map %>% 
    filter(program == 'Office of Family Violence Prevention and Response')
  
  OFFICE_NAME <- unique(program_map$program)
  
  OFVPS_terms <- c('violence')
  
  OFVPS_prog_acts <- grantee_df %>% 
    filter(grepl(OFVPS_terms, program_activities_funding_this_award, 
                 ignore.case = TRUE)) %>% 
    mutate(cfda_flag = ifelse(grepl(OFVPS_terms, 
                                    program_activities_funding_this_award, 
                                    ignore.case = TRUE),1, 0), 
           funding_office_flag =ifelse( grepl(OFVPS_terms, 
                                              funding_office_name, 
                                              ignore.case = TRUE), 1, 0),
           prog_activities_flag = ifelse(grepl(OFVPS_terms, 
                                               program_activities_funding_this_award, 
                                               ignore.case = TRUE), 1, 0)
    ) %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag, 
                                                prog_activities_flag)))/3,
           assigned_office = OFFICE_NAME) %>% 
    arrange(assistance_transaction_unique_key)
  
  return(OFVPS_prog_acts)
  
}

# test_df <- get_OFVPS_prog_acts_meth(grantee_df = full_grants_df)
# table(test_df$cfda_title)

#-------------------------------------------------------------------------------
##### Method 4: function to subset the grants data based off on the 
# `funding_office_name` variable 
#-------------------------------------------------------------------------------
get_OFVPS_funding_meth <- function(grantee_df) {
  
  OFVPS_terms <- c('violence')
  
  OFVPS_funding_df <- grantee_df %>% 
    filter(grepl(OFVPS_terms, funding_office_name, ignore.case = TRUE)) %>% 
    mutate(assigned_office = 'Office of Family Violence Prevention and Response') %>% 
    mutate(cfda_flag = ifelse(grepl(OFVPS_terms, 
                                    program_activities_funding_this_award, 
                                    ignore.case = TRUE),1, 0), 
           funding_office_flag =ifelse( grepl(OFVPS_terms, 
                                              funding_office_name, 
                                              ignore.case = TRUE), 1, 0),
           prog_activities_flag = ifelse(grepl(OFVPS_terms, 
                                               program_activities_funding_this_award, 
                                               ignore.case = TRUE), 1, 0)
    ) %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag,
                                                prog_activities_flag)))/3) %>% 
    arrange(assistance_transaction_unique_key)
  
  return(OFVPS_funding_df) 
  
}

# test_df <- get_OFVPS_funding_meth(grantee_df = full_grants_df)
# table(test_df$cfda_title)

# -----------------------------------------------------------------------------
#### Method 5: get_OFVPS_combo_meth(): function that uses a combo of the other 
# four methods to output a more reliable list of relevant grants 
# -----------------------------------------------------------------------------
get_OFVPS_combo_meth <- function(grantee_df = full_grants_df) {
  
  program_map <- acf_program_map %>% 
    filter(program == 'Office of Family Violence Prevention and Response')
  
  OFFICE_NAME <- unique(program_map$program)
  
  # Apply FAIN ID location for Discretionary grant types
  # -----------------------------------------------------
  discretionary_codes <- program_map %>% 
    filter(grant_program_type == 'Discretionary')
  
  program_codes_disc <- discretionary_codes$grant_program %>%
    str_split('-') %>% 
    sapply("[", 1) %>% 
    trimws()
  
  # Construct the regex pattern to match [numeric][alpha][numeric] 
  # Apply only to discretionary programs 
  pc_regex_disc <- paste0('(', paste(program_codes_disc, collapse = "|"), ')')
  complete_regex_disc <- paste0("[0-9]+", pc_regex_disc, "[0-9]+")
  
  # Apply FAIN ID location for Mandatory grant types  
  # -----------------------------------------------------
  mandatory_codes <- program_map %>% 
    filter(grant_program_type == 'Mandatory')
  program_codes_mand <- mandatory_codes$grant_program %>%
    str_split('-') %>% 
    sapply("[", 1) %>% 
    trimws()
  # browser()
  
  # Construct the regex pattern to match [numeric][alpha][numeric] 
  # Apply only to discretionary programs 
  complete_regex_mand <- paste0(paste0(program_codes_mand, collapse = "$|"), "$")
  
  # Flag corresponding matches 
  regex_fain_mand <- grepl(complete_regex_mand, grantee_df$award_id_fain, perl = TRUE)
  regex_fain_disc <- grepl(complete_regex_disc, grantee_df$award_id_fain, perl = TRUE)
  
  # Join cfda_title flagged obs 
  OFVPS_terms <- c('violence')
  OFVPS_cfda <- grantee_df %>% 
    filter(grepl(OFVPS_terms, cfda_title, ignore.case = TRUE))
  
  # Create our output dataframe 
  combo_df <- rbind(grantee_df[regex_fain_mand,], grantee_df[regex_fain_disc,],
                  OFVPS_cfda) %>% 
    unique() %>% 
    mutate(cfda_flag = ifelse(grepl(OFVPS_terms, cfda_title, 
                                    ignore.case = TRUE),1, 0), 
           funding_office_flag =ifelse( grepl(OFVPS_terms, 
                                              funding_office_name, 
                                              ignore.case = TRUE), 1, 0),
           prog_activities_flag = ifelse(grepl(OFVPS_terms, 
                                               program_activities_funding_this_award, 
                                               ignore.case = TRUE), 1, 0)
    ) %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag,
                                                prog_activities_flag)))/3,
           assigned_office = OFFICE_NAME) %>% 
    arrange(assistance_transaction_unique_key)
  
  return(combo_df)
  
}

# -----------------------------------------------------------------------------


