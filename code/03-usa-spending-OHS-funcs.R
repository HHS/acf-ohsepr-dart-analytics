#### USA Spending - Office of Head Start Functions 
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

###############################################################################
# Office of Head Start
###############################################################################


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
