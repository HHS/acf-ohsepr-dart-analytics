#### USA Spending - Offices Functions 
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

####: get_full_grants_df: function to pull in the most recent full_grants_df if 
# it is not already in memory 
get_full_grants_df <- function() {
  
  if (exists("full_grants_df") == FALSE) {
  # browser()
  max_date <- USASPENDING_dir %>% list.files() %>% 
    substr(18,23) %>% max()
  
  file_name <- paste0(USASPENDING_dir, '/full_grants_data_', 
                      max_date, '.feather')
  
  print(USASPENDING_dir)
  df = read_feather(file_name)
  cat('The full_grants_df was successfully pulled.') 
  
  return(df)}
  
  if (exists("full_grants_df") == TRUE) {
    cat('The full_grants_df already exists in memory.') 
    
  }
}

###############################################################################
# Office of Head Start
###############################################################################

#### prime_get_OHS(): RETIRED function to subset to head start data 
prime_get_OHS <- function(grantee_df) {
  
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

ohs <- prime_get_OHS(usa_spending_df)

contains_foster_care <- ohs[grepl("FOSTER CARE", ohs$program_activities_funding_this_award, ignore.case = TRUE),]
# getting pc 


ohs_office_name <- ohs %>% 
  filter(funding_office_name == "ACF HEAD START")
ohs_cfda <- ohs %>% 
  filter(cfda_title == "HEAD START")

setdiff_str <- setdiff(ohs_cfda$award_id_fain, ohs_office_name$award_id_fain)

diff_set <- ohs %>% 
  filter(award_id_fain %in% setdiff_str)
View(diff_set)

diff_set %>% filter(funding_office_name %in% c("ACF OFFICE OF REFUGEE RESETTLEMENT")) %>% View()

# 04CH0170 is listed under ORR but should be OHS 

# 04CD004053
diff_set %>% filter(award_id_fain == "04CD004053") %>% pull(funding_office_name)


#  to do: check diff set; summarize
  #  implement get_OHS() to be specific to "04CD004053" format 
###############################################################################
# Office of Family Assistance
###############################################################################

# function to take in grantee df and output only OFA observations 
prime_get_OFA <- function(grantee_df) {
  
  program_map <- acf_program_map %>% 
    filter(program == 'Office of Family Assistance')
  OFFICE_NAME <- unique(program_map$program)
  
  program_codes <- str_split(program_map$grant_program, "-") %>%
    sapply("[", 1) %>% 
    trimws()
  
  print(program_codes)
  
  pc_regex <- program_codes %>% paste(collapse = "|")
  regex_fain <- grepl(pc_regex, grantee_df$award_id_fain)
  out_df <- grantee_df[regex_fain,] %>% 
    mutate(assigned_office = OFFICE_NAME)
  return(out_df)
  
}

prime_OFA_df <- prime_get_OFA(usa_spending_df)

prime_active_OFA_df <- prime_OFA_df %>% 
  filter(CURR_DATE <= period_of_performance_current_end_date)
dim(prime_active_OFA_df)
active_OFA_list <- unique(prime_OFA_df$award_id_fain)
length(active_OFA_list) # 3126


prime_active_OFA_df <- prime_active_OFA_df %>% 
  group_by(award_id_fain, recipient_address_line_1, recipient_address_line_2,
           recipient_zip_code) %>% 
  mutate(facility_id = 1:n())

length(unique(prime_active_OFA_df$award_id_fain)) #500
length(unique(prime_active_OFA_df$facility_id)) # 16

###############################################################################
# Office of Family Violence Prevention and Response
###############################################################################

# function to take in grantee df and output only OFVPS observations 
prime_get_OFVPS <- function(grantee_df) {
  
  program_map <- acf_program_map %>% 
    filter(program == 'Office of Family Violence Prevention and Response')
  # browser()
  OFFICE_NAME <- unique(program_map$program)
  program_codes <- str_split(program_map$grant_program_code, "-") %>%
    sapply("[", 1) %>% 
    trimws()
  print(list(program_codes))
  pc_regex <- program_codes %>% paste(collapse = "|")
  regex_fain <- grepl(pc_regex, grantee_df$award_id_fain)
  out_df <- grantee_df[regex_fain,] %>% 
    mutate(assigned_office = OFFICE_NAME)
  return(out_df)
  
}

prime_OFVPS_df <- prime_get_OFVPS(usa_spending_df)



###############################################################################
# ANA 
###############################################################################

# function to take in grantee df and output only OFVPS observations 
prime_get_ANA <- function(grantee_df) {
  
  program_map <- acf_program_map %>% 
    filter(program == 'Native Americans')
  # browser()
  OFFICE_NAME <- unique(program_map$program)
  program_codes <- str_split(program_map$grant_program_code, "-") %>%
    sapply("[", 1) %>% 
    trimws()
  print(list(program_codes))
  pc_regex <- program_codes %>% paste(collapse = "|")
  regex_fain <- grepl(pc_regex, grantee_df$award_id_fain)
  out_df <- grantee_df[regex_fain,] %>% 
    mutate(assigned_office = OFFICE_NAME)
  return(out_df)
  
}

ana_pull <- prime_get_ANA(usa_spending_df)

OUT_FOLDER

# function to write by office output 
write_by_office <- function(office_df, ...) {
  
  # browser()
  OFFICE_NAME <- unique(office_df$assigned_office)
  print(OFFICE_NAME)
  # output in feather format 
  OUT_FEATHER <- paste(sep = "/", OUT_FOLDER, paste(OFFICE_NAME, '.feather'))
  write_feather(x = office_df, OUT_FEATHER)
  # output in csv format
  OUT_CSV <- paste(sep = "/", OUT_FOLDER, paste(OFFICE_NAME, '.csv'))
  write.csv(x = office_df, OUT_CSV)
  
}



#  write by office 
write_by_office(ana_pull)
write_by_office(prime_OFVPS_df)
write_by_office(prime_OFA_df)
write_by_office(ohs)
