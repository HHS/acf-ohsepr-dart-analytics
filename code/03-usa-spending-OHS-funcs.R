#### USA Spending - Office of Head Start Functions 
#-------------------------------------------------------------------------------
# Purpose:
#-------------------------------------------------------------------------------
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
    browser()
    max_date <- HS_dir %>% list.files() %>% 
      substr(22, 31) %>% max()
    
    file_name <- paste0(HS_dir, '/cleaned_headstart', 
                        max_date, '.feather')
    
    df = read_feather(file_name)
    cat('The cleaned_headstart (OHS) dataframe was successfully pulled.') 
    
    return(df)}
  
  if (exists("cleaned_headstart") == TRUE) {
    cat('The cleaned_headstart dataframe already exists in memory.') 
    
  }
}

#-------------------------------------------------------------------------------
##### Method 3: regex on "Head Start" program_activities_funding_this_award  
get_OHS_prog_acts_meth <- function(grantee_df) {
  browser()
  program_map <- acf_program_map %>% 
    filter(program == 'Head Start')
  
  OFFICE_NAME <- unique(program_map$program)
  
  OHS_prog_acts <- grantee_df %>% 
    filter(grepl("Head Start", program_activities_funding_this_award, ignore.case = TRUE)) %>% 
    mutate(cfda_flag = ifelse(grepl("Head Start", program_activities_funding_this_award, ignore.case = TRUE),1, 0), 
           funding_office_flag =ifelse( grepl("Head Start", funding_office_name, ignore.case = TRUE), 1, 0),
           prog_activities_flag = ifelse(grepl("Head Start", program_activities_funding_this_award, ignore.case = TRUE), 1, 0)
    ) %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag, prog_activities_flag)))/3)
  
  
  OHS_cfda <- OHS_cfda %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag, prog_activities_flag)))/3) %>% 
    mutate(assigned_office = OFFICE_NAME)
  
  return(OHS_cfda)
  
}

test_df <- get_OHS_prog_acts_meth(grantee_df = full_grants_df)
dim(test_df) == dim(ohs_conf)
# -----------------------------------------------------------------------------

##### Method : regex on "Head Start" CFDA  
# function to take in head start data 
get_OHS_cfda_meth <- function(grantee_df) {
  
  program_map <- acf_program_map %>% 
    filter(program == 'Head Start')
  
  OFFICE_NAME <- unique(program_map$program)
  
  OHS_cfda <- grantee_df %>% 
    filter(grepl("Head Start", cfda_title, ignore.case = TRUE)) %>% 
    mutate(cfda_flag = ifelse(grepl("Head Start", cfda_title, ignore.case = TRUE),1, 0), 
           funding_office_flag =ifelse( grepl("Head Start", funding_office_name, ignore.case = TRUE), 1, 0),
           prog_activities_flag = ifelse(grepl("Head Start", program_activities_funding_this_award, ignore.case = TRUE), 1, 0)
    ) %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag, prog_activities_flag)))/3)
  
  
  OHS_cfda <- OHS_cfda %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag, prog_activities_flag)))/3) %>% 
    mutate(assigned_office = OFFICE_NAME)
  
  return(OHS_cfda)
  
}

test_df <- get_OHS_cfda_meth(usa_spending_df)
dim(test_df) == dim(ohs_conf)
# -----------------------------------------------------------------------------
get_OHS_combo_meth <- function(grantee_df) {
  
  program_map <- acf_program_map %>% 
    filter(program == 'Head Start')
  
  OFFICE_NAME <- unique(program_map$program)
  
  OHS_cfda <- grantee_df %>% 
    # filter(grepl("Head Start", cfda_title, ignore.case = TRUE)) %>% 
    mutate(cfda_flag = ifelse(grepl("Head Start", cfda_title, ignore.case = TRUE),1, 0), 
           funding_office_flag =ifelse( grepl("Head Start", funding_office_name, ignore.case = TRUE), 1, 0),
           prog_activities_flag = ifelse(grepl("Head Start", program_activities_funding_this_award, ignore.case = TRUE), 1, 0)
    ) %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag, prog_activities_flag)))/3)
  
  
  OHS_cfda <- OHS_cfda %>% 
    mutate(office_confidence = rowSums(across(c(cfda_flag,funding_office_flag, prog_activities_flag)))/3) %>% 
    mutate(assigned_office = OFFICE_NAME) %>% 
    filter(cfda_flag == 1 | funding_office_flag == 1 | prog_activities_flag == 1)
  
  return(OHS_cfda)
  
}

combo_test_df <- get_OHS_combo_meth(usa_spending_df)
dim(test_df) == dim(ohs_conf)
# -----------------------------------------------------------------------------


##### Method : using 
# function to take in head start data 
get_OHS_pc_codes_meth <- function(grantee_df) {
  
  program_map <- acf_program_map %>% 
    filter(program == 'Head Start')
  
  OFFICE_NAME <- unique(program_map$program)
  
  # browser()
  program_codes <- program_map$grant_program %>%
    str_split('-') %>% 
    sapply("[", 1) %>% 
    trimws()
  print(program_codes)
  # pc_regex <- program_codes %>% paste(collapse = "|")
  
  # Construct the regex pattern to match [numeric][alpha][numeric]
  pc_regex <- paste0('(', paste(program_codes, collapse = "|"), ')')
  complete_regex <- paste0("[0-9]+", pc_regex, "[0-9]+")
  
  
  # Apply the regex pattern to filter the dataframe
  regex_fain <- grepl(complete_regex, grantee_df$award_id_fain, perl = TRUE)
  
  # print(grantee_df$award_id_fain[regex_fain])
  
  out_df <- grantee_df[regex_fain,] %>% 
    mutate(assigned_office = OFFICE_NAME)
  
  return(out_df)
  
}

get_OHS_pc_codes_meth()

# -----------------------------------------------------------------------------







# -----------------------------------------------------------------------------
