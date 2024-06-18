#### USA Spending Data - Offices 
#-------------------------------------------------------------------------------
# Purpose: This file subsets the input grants data into our three target offices:
# 1) Office of Head Start
# 2) Office of Family Violence Prevention and Services 
# 3) Office of Family Assistance 

# There are several possible methods for subsetting the data. These are:
# 1) ACF Program Codes regex filtering - using ACF program  codes
# 2) `CFDA title` regex filtering - e.g., "head start"
# 3) `program activities` code regex filtering - e.g., "head start"
# 4) `funding_office_name` regex filtering - e.g.,"head start" 
# 5) combo method combining 2, 3 *and* 4! 
#-------------------------------------------------------------------------------

# Source USA Spending Grants data, if full_grants_df does not exist
get_full_grants_df()

# Office of Head Start Methods:
#-------------------------------------------------------------------------------

# Source OHS Facilities data, if cleaned_headstart does not exist
get_head_start_df()

# Source OHS functions
source(paste0(CODE_DIR, "/03-usa-spending-OHS-funcs.R" ))

# 1) ACF Program Codes regex filtering - using ACF program  codes
OHS_pccodes_df <- get_OHS_pc_codes_meth(grantee_df = full_grants_df)
table(OHS_pccodes_df$cfda_title)
# 2) `CFDA title` regex filtering - e.g., "head start"
OHS_cfda_df <- get_OHS_cfda_meth(grantee_df = full_grants_df)

# 3) `program activities` code regex filtering - e.g., "head start"
OHS_prog_acts_df <- get_OHS_prog_acts_meth(grantee_df = full_grants_df)

# 4) `funding_office_name` regex filtering - e.g.,"head start" 
OHS_funding_office_df <- full_grants_df %>% 
  filter(grepl("Head Start", funding_office_name, ignore.case = TRUE))
table(OHS_funding_office_df$funding_office_name)

# 5) combo method combining 2, 3 *and* 4! 
OHS_combo_df <- get_OHS_combo_meth(grantee_df = full_grants_df)

table(OHS_combo_df$cfda_title)
# to do:
# [] summarize_office_methods() 

#### summarize_office_methods():
summarize_office_methods <- function(pc_df, cfda_df, prog_acts_df, 
                                     funding_office_df, combo_df) {
  
  
}

summarize_office_methods(pc_df = OHS_pccodes_df, cfda_df = OHS_cfda_df, 
                         funding_office_df = OHS_funding_office_df, 
                         combo_df = OHS_combo_df)

# Office of Family Violence Prevention and Services 
#-------------------------------------------------------------------------------


# 1) ACF Program Codes regex filtering - using ACF program  codes
# 2) `CFDA title` regex filtering - e.g., "head start"
# 3) `program activities` code regex filtering - e.g., "head start"
# 4) `funding_office_name` regex filtering - e.g.,"head start" 
# 5) combo method combining 2, 3 *and* 4! 
source(paste0(CODE_DIR, "/03-usa-spending-OFVPS-funcs.R" ))

prime_OFVPS_df <- prime_get_OFVPS(full_grants_df)

exclude <- c('LOW-INCOME HOME ENERGY ASSISTANCE',
             'CHILD CARE MANDATORY AND MATCHING FUNDS OF THE CHILD CARE AND DEVELOPMENT FUND',
             'CHILD CARE AND DEVELOPMENT BLOCK GRANT')
View(prime_OFVPS_df)

out <- prime_OFVPS_df %>% 
  filter(!cfda_title %in% exclude,
         period_of_performance_current_end_date > CURR_DATE)
table(out$funding_office_name)
dim(out)
write.xlsx(x = out, 
           file = "C:/Users/Bela Reeves/Documents/@Raft - 2023 and after/@task-3/OFVPS-2024-06-14.xlsx")
# Office of Family Assistance 
#-------------------------------------------------------------------------------
source(paste0(CODE_DIR, "/03-usa-spending-OFA-funcs.R" ))



# 1) ACF Program Codes regex filtering - using ACF program  codes
# 2) `CFDA title` regex filtering - e.g., "head start"
# 3) `program activities` code regex filtering - e.g., "head start"
# 4) `funding_office_name` regex filtering - e.g.,"head start" 
# 5) combo method combining 2, 3 *and* 4! 


