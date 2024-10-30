#### USA Spending Data - Programs
#####~~~~~#####~~~~~#####~~~~~#####~~~~~#####~~~~~#####~~~~~#####~~~~~#####~~~~~

## source relevant functions 


## Data read in
# identifying most recent output 
MAPPING_file_dir <- paste0("/",max(sort(list.files(OUT_DIR))))
MAPPING_file_dir

# acf program mapping data
mapping_loc <- paste0(OUT_DIR, MAPPING_file_dir, "/acf_program_mapping.feather")
mapping_loc
file.exists(mapping_loc)

acf_program_map <- read_feather(mapping_loc)

# usa spending data 
grants_loc <- paste0(OUT_DIR, MAPPING_file_dir, "/usa_spending_grants_data.feather")
grants_loc
file.exists(grants_loc)


usa_spending_df <- read_feather(grants_loc)

## program specific data
# OHS
prime_OHS_df <- prime_get_OHS(usa_spending_df) %>% 
  filter(CURR_DATE <= period_of_performance_current_end_date)
dim(prime_OHS_df)

table(prime_OHS_df$program_activities_funding_this_award) %>% as.data.frame() %>% 
  View()

acf_program_map %>% filter(program == "Head Start") %>% View()

contains_foster_care <- prime_OHS_df[grepl("FOSTER CARE", prime_OHS_df$program_activities_funding_this_award, ignore.case = TRUE),]

#OFA

#OFVPS
