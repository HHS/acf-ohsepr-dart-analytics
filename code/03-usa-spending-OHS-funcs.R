#### USA Spending - Office of Head Start Functions 
#-------------------------------------------------------------------------------
# Purpose: To apply our 5 methods of data subsetting to our full_grants_df 
# to more accurately identify OHS grants.
# 5 methods are below:
# 1) ACF Program Codes regex filtering - using ACF program  codes
# 2) `CFDA title` regex filtering - e.g., "head start"
# 3) `program activities` code regex filtering - e.g., "head start"
# 4) `funding_office_name` regex filtering - e.g.,"head start" 
# 5) combo method combining 2, 3 *and* 4! 

#-------------------------------------------------------------------------------
# Office of Head Start
#-------------------------------------------------------------------------------

#### get_head_start_df(): function to get the mot recent cleaned head start data
get_head_start_df <- function() {
  
  if (exists('cleaned_headstart') == FALSE) {
    # browser()
    HS_dir <- paste0(getwd(), '/inputs/head-start-data')
    max_date <- HS_dir %>% list.files() %>% 
      substr(22, 31) %>% max()
    HS_dir %>% list.files() %>% print()
    file_name <- paste0(HS_dir, '/head_start_locations_', 
                        max_date, '.feather')
    
    df = read_feather(file_name)
    assign(x = 'cleaned_headstart', value = df, envir = .GlobalEnv)
    
    cat('The cleaned_headstart (OHS) dataframe was successfully pulled.') 
    
    return(df)}
  
  if (exists("cleaned_headstart") == TRUE) {
    cat('The cleaned_headstart dataframe already exists in memory.') 
    
  }
}

# get_head_start_df()

#-------------------------------------------------------------------------------
##### Method 1: get_OHS_pc_codes_meth(): function to subset our grants data to
# only grants that contain Head Start program codes as identified in the 
# ACF Program Maps input dataset 
#-------------------------------------------------------------------------------
get_OHS_pc_codes_meth <- function(grantee_df = full_grants_df) {
  
  program_map <- acf_program_map %>% 
    filter(program == 'Head Start')
  
  OFFICE_NAME <- unique(program_map$program)
  
  program_codes <- program_map$grant_program %>%
    str_split('-') %>% 
    sapply("[", 1) %>% 
    trimws()

  # Construct the regex pattern to match [numeric][alpha][numeric]
    # None are Mandatory grants so this logic holds for all grants
  pc_regex <- paste0('(', paste(program_codes, collapse = "|"), ')')
  complete_regex <- paste0("[0-9]+", pc_regex, "[0-9]+")
  
  # Apply the regex pattern to filter the dataframe
  regex_fain <- grepl(complete_regex, grantee_df$award_id_fain, perl = TRUE)
  
  # Construct our output dataframe 
  out_df <- grantee_df[regex_fain,] %>% 
    mutate(cfda_flag = ifelse(grepl("Head Start", cfda_title, 
                                    ignore.case = TRUE),1, 0), 
           funding_office_flag =ifelse( grepl("Head Start", 
                                              funding_office_name, 
                                              ignore.case = TRUE), 1, 0),
           prog_activities_flag = ifelse(grepl("Head Start", 
                                               program_activities_funding_this_award, 
                                               ignore.case = TRUE), 1, 0)
    ) %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag,
                                                prog_activities_flag)))/3,
           assigned_office = OFFICE_NAME)  %>% 
    arrange(assistance_transaction_unique_key)
  
  return(out_df)
  
}

# test_df <- get_OHS_pc_codes_meth()
# table(test_df$cfda_title)

# ------------------------------------------------------------------------------
##### Method 2: get_OHS_cfda_meth(): function to subset the grants data based 
# off of a regex on "Head Start" CFDA
get_OHS_cfda_meth <- function(grantee_df = full_grants_df) {
  
  program_map <- acf_program_map %>% 
    filter(program == 'Head Start')
  
  OFFICE_NAME <- unique(program_map$program)
  
  OHS_cfda <- grantee_df %>% 
    filter(grepl("Head Start", cfda_title, ignore.case = TRUE)) %>% 
    mutate(cfda_flag = ifelse(grepl("Head Start", cfda_title, 
                                    ignore.case = TRUE),1, 0), 
           funding_office_flag =ifelse( grepl("Head Start", 
                                              funding_office_name, 
                                              ignore.case = TRUE), 1, 0),
           prog_activities_flag = ifelse(grepl("Head Start", 
                                               program_activities_funding_this_award, 
                                               ignore.case = TRUE), 1, 0)
    ) %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag, 
                                                prog_activities_flag)))/3,
           assigned_office = OFFICE_NAME)  %>% 
    arrange(assistance_transaction_unique_key)
  
  return(OHS_cfda)
  
}

test_df <- get_OHS_cfda_meth()
table(test_df$cfda_title)
#-------------------------------------------------------------------------------
##### Method 3: get_OHS_prog_acts_meth(): function that applies a 
# regex on "Head Start" program_activities_funding_this_award  
#-------------------------------------------------------------------------------
get_OHS_prog_acts_meth <- function(grantee_df = full_grants_df) {
  # browser()
  program_map <- acf_program_map %>% 
    filter(program == 'Head Start')
  
  OFFICE_NAME <- unique(program_map$program)
  
  OHS_prog_acts <- grantee_df %>% 
    filter(grepl("Head Start", program_activities_funding_this_award, 
                 ignore.case = TRUE)) %>% 
    mutate(cfda_flag = ifelse(grepl("Head Start", 
                                    program_activities_funding_this_award, 
                                    ignore.case = TRUE),1, 0), 
           funding_office_flag =ifelse( grepl("Head Start", 
                                              funding_office_name, 
                                              ignore.case = TRUE), 1, 0),
           prog_activities_flag = ifelse(grepl("Head Start", 
                                               program_activities_funding_this_award, 
                                               ignore.case = TRUE), 1, 0)
    ) %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag, 
                                                prog_activities_flag)))/3,
           assigned_office = OFFICE_NAME)  %>% 
    arrange(assistance_transaction_unique_key)
  
  return(OHS_prog_acts)
  
}
test_df <- get_OHS_prog_acts_meth(grantee_df = full_grants_df)
table(test_df$cfda_title)

#-------------------------------------------------------------------------------
##### Method 4: function to subset the grants data based off on the 
# `funding_office_name` variable 
#-------------------------------------------------------------------------------
get_OHS_funding_meth <- function(grantee_df) {
  OHS_funding_df <- grantee_df %>% 
    filter(grepl("Head Start", funding_office_name, ignore.case = TRUE)) %>% 
    mutate(assigned_office = 'Head Start') %>% 
    mutate(cfda_flag = ifelse(grepl("Head Start", 
                                    program_activities_funding_this_award, 
                                    ignore.case = TRUE),1, 0), 
           funding_office_flag =ifelse( grepl("Head Start", 
                                              funding_office_name, 
                                              ignore.case = TRUE), 1, 0),
           prog_activities_flag = ifelse(grepl("Head Start", 
                                               program_activities_funding_this_award, 
                                               ignore.case = TRUE), 1, 0)
    ) %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag,
                                                prog_activities_flag)))/3)  %>% 
    arrange(assistance_transaction_unique_key)
  
  return(OHS_funding_df)
  
}

# test_df <- get_OHS_funding_meth(grantee_df = full_grants_df)
# table(test_df$cfda_title)

# -----------------------------------------------------------------------------
#### Method 5: get_OHS_combo_meth(): function that uses a combo of the other 
# four methods to output a more reliable list of relevant grants 
# -----------------------------------------------------------------------------
get_OHS_combo_meth <- function(grantee_df = full_grants_df) {
  
  program_map <- acf_program_map %>% 
    filter(program == 'Head Start')
  
  OFFICE_NAME <- unique(program_map$program)
  
  # browser()
  program_codes <- program_map$grant_program %>%
    str_split('-') %>% 
    sapply("[", 1) %>% 
    trimws()
  # print(program_codes)
  # pc_regex <- program_codes %>% paste(collapse = "|")
  
  # Construct the regex pattern to match [numeric][alpha][numeric]
  pc_regex <- paste0('(', paste(program_codes, collapse = "|"), ')')
  complete_regex <- paste0("[0-9]+", pc_regex, "[0-9]+")
  
  # Apply the regex pattern to filter the dataframe
  regex_fain <- grepl(complete_regex, grantee_df$award_id_fain, perl = TRUE)
  
  # print(grantee_df$award_id_fain[regex_fain])
  
  # Identify cfda_titles to exclude based on working session
  NO_CFDA <- c('child support', 'marylee')
  
  combo_df <- grantee_df[regex_fain,] %>% 
    # Remove those `cfda_title` obs before computing the flags 
    filter(!grepl(paste(NO_CFDA, collapse = "|"), cfda_title, 
                  ignore.case = TRUE)) %>% 
    mutate(cfda_flag = ifelse(grepl("Head Start", cfda_title, 
                                    ignore.case = TRUE),1, 0), 
           funding_office_flag =ifelse( grepl("Head Start", 
                                              funding_office_name, 
                                              ignore.case = TRUE), 1, 0),
           prog_activities_flag = ifelse(grepl("Head Start", 
                                               program_activities_funding_this_award, 
                                               ignore.case = TRUE), 1, 0)
    ) %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag,
                                                prog_activities_flag)))/3,
           assigned_office = OFFICE_NAME) %>% 
    arrange(assistance_transaction_unique_key)
  
  return(combo_df)
  
}


# combo_test_df <- get_OHS_combo_meth()
# table(combo_test_df$cfda_title)
# -----------------------------------------------------------------------------
