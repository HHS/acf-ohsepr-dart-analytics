#### USA Spending Data - Offices 
#-------------------------------------------------------------------------------
# Purpose: This file subsets the input grants data into our three target offices:
# 1) Office of Head Start
# 2) Office of Family Violence Prevention and Services 
# 3) Office of Family Assistance 

# Office of Head Start Methods:
# There are several possible methods for subsetting the data. These are:
# 1) ACF Program Codes regex filtering - using ACF program  codes
# 2) `CFDA title` regex filtering - e.g., "head start"
# 3) `program activities` code regex filtering - e.g., "head start"
# 4) `funding_office_name` regex filtering - e.g.,"head start" 
# 5) combo method combining 2, 3 *and* 4! 
#-------------------------------------------------------------------------------

# Source USA Spending Grants data, if full_grants_df does not exist
get_full_grants_df()

#-------------------------------------------------------------------------------

# Source OHS Facilities data, if cleaned_headstart does not exist
get_head_start_df()

# Source ACF Program map data, if acf_program_map does not exist 
if (exists('acf_program_map') == FALSE) {
  ACF_source <- paste0(CODE_DIR, "/02-acf-program-map.R")
  file.exists(ACF_source)
  source(ACF_source)
}

# Source OHS functions
source(paste0(CODE_DIR, "/03-usa-spending-OHS-funcs.R" ))

# 1) ACF Program Codes regex filtering - using ACF program  codes
OHS_pccodes_df <- get_OHS_pc_codes_meth(grantee_df = full_grants_df)
table(OHS_pccodes_df$program_activities_funding_this_award)
table(OHS_pccodes_df$cfda_title)

# 2) `CFDA title` regex filtering - e.g., "head start"
OHS_cfda_df <- get_OHS_cfda_meth(grantee_df = full_grants_df)
table(OHS_cfda_df$program_activities_funding_this_award)
table(OHS_cfda_df$cfda_title)

# 3) `program activities` code regex filtering - e.g., "head start"
OHS_prog_acts_df <- get_OHS_prog_acts_meth(grantee_df = full_grants_df)
table(OHS_prog_acts_df$program_activities_funding_this_award)
table(OHS_prog_acts_df$cfda_title)

# 4) `funding_office_name` regex filtering - e.g.,"head start" 
OHS_funding_office_df <- get_OHS_funding_meth(grantee_df = full_grants_df)
table(OHS_funding_office_df$program_activities_funding_this_award)
table(OHS_funding_office_df$cfda_title)

# 5) combo method combining 2, 3 *and* 4! 
OHS_combo_df <- get_OHS_combo_meth(grantee_df = full_grants_df)
table(OHS_combo_df$program_activities_funding_this_award)
table(OHS_combo_df$cfda_title)

# Evaluate all the methods applied to OHS. Intentionally in alphabetical order. 
OHS_list <- list(OHS_cfda_df, OHS_combo_df, OHS_funding_office_df, 
                 OHS_pccodes_df, OHS_prog_acts_df)

# OHS_list
names(OHS_list) # expect it to be NULL initially 
 
ohs_objects_names <- ls()[grepl("^OHS.*df$", ls())] %>% sort()
ohs_objects_names 
names(OHS_list) <- ohs_objects_names
names(OHS_list)

# Output a summary table of methods applied to OHS 
OHS_combined_summary <- do.call(rbind, lapply(OHS_list, summarize_office_methods))
OHS_combined_summary

OHS_combined_summary$office <- 'OHS'
OHS_combined_summary$meth <- names(OHS_list)
OHS_combined_summary$meth

# Office of Family Violence Prevention and Services 
#-------------------------------------------------------------------------------

# Source OFVPS functions 
source(paste0(CODE_DIR, "/03-usa-spending-OFVPS-funcs.R" ))

# 1) ACF Program Codes regex filtering - using ACF program  codes
OFVPS_pccodes_df <- get_OFVPS_pc_codes_meth(grantee_df = full_grants_df)
table(OFVPS_pccodes_df$program_activities_funding_this_award)
table(OFVPS_pccodes_df$cfda_title)
nrow(OFVPS_pccodes_df)

# 2) `CFDA title` regex filtering - e.g., "Family Violence"
OFVPS_cfda_df <- get_OFVPS_cfda_meth(grantee_df = full_grants_df)
table(OFVPS_cfda_df$program_activities_funding_this_award)
table(OFVPS_cfda_df$cfda_title)
nrow(OFVPS_cfda_df)

# 3) `program activities` code regex filtering - e.g., 'violence'
OFVPS_prog_acts_df <- get_OFVPS_prog_acts_meth(grantee_df = full_grants_df)
table(OFVPS_prog_acts_df$program_activities_funding_this_award)
table(OFVPS_prog_acts_df$cfda_title)
nrow(OFVPS_prog_acts_df)

# 4) `funding_office_name` regex filtering - e.g., 'violence'
OFVPS_funding_office_df <- get_OFVPS_funding_meth(grantee_df = full_grants_df)
table(OFVPS_funding_office_df$program_activities_funding_this_award)
table(OFVPS_funding_office_df$cfda_title)
nrow(OFVPS_funding_office_df) # There is no OFVPS funding office in the data 

# 5) combo method combining 2, 3 *and* 4! 
OFVPS_combo_df <- get_OFVPS_combo_meth(grantee_df = full_grants_df)
table(OFVPS_combo_df$program_activities_funding_this_award)
table(OFVPS_combo_df$cfda_title)
nrow(OFVPS_combo_df)

# Evaluate all the methods applied to OHS. Intentionally in alphabetical order. 
OFVPS_list <- list(OFVPS_cfda_df, OFVPS_combo_df, OFVPS_funding_office_df,
                   OFVPS_pccodes_df, OFVPS_prog_acts_df)
OFVPS_list
# OFVPS_list
names(OFVPS_list) # expect it to be NULL initially

ofvps_objects_names <- ls()[grepl("^OFVPS.*df$", ls())] %>% sort()
ofvps_objects_names 
names(OFVPS_list) <- ofvps_objects_names
names(OFVPS_list)

# Output a summary table of methods applied to OFVPS 
OFVPS_combined_summary <- do.call(rbind, lapply(OFVPS_list, summarize_office_methods))
OFVPS_combined_summary

OFVPS_combined_summary$office <- 'OFVPS'
OFVPS_combined_summary$meth <- names(OFVPS_list)
OFVPS_combined_summary 

# Office of Family Assistance 
#-------------------------------------------------------------------------------
source(paste0(CODE_DIR, "/03-usa-spending-OFA-funcs.R" ))

# 1) ACF Program Codes regex filtering - using ACF program  codes
OFA_pccodes_df <- get_OFA_pc_codes_meth(grantee_df = full_grants_df)
table(OFA_pccodes_df$program_activities_funding_this_award)
table(OFA_pccodes_df$cfda_title)
nrow(OFA_pccodes_df)

# 2) `CFDA title` regex filtering - e.g., "Family Violence"
OFA_cfda_df <- get_OFA_cfda_meth(grantee_df = full_grants_df)
table(OFA_cfda_df$program_activities_funding_this_award)
table(OFA_cfda_df$cfda_title)
nrow(OFA_cfda_df)

# 3) `program activities` code regex filtering - e.g., 'violence'
OFA_prog_acts_df <- get_OFA_prog_acts_meth(grantee_df = full_grants_df)
table(OFA_prog_acts_df$program_activities_funding_this_award)
table(OFA_prog_acts_df$cfda_title)
nrow(OFA_prog_acts_df)

# 4) `funding_office_name` regex filtering - e.g., 'violence'
OFA_funding_office_df <- get_OFA_funding_meth(grantee_df = full_grants_df)
table(OFA_funding_office_df$program_activities_funding_this_award)
table(OFA_funding_office_df$cfda_title)
nrow(OFA_funding_office_df) 

# 5) combo method combining 2, 3 *and* 4! 
OFA_combo_df <- get_OFA_combo_meth(grantee_df = full_grants_df)
table(OFA_combo_df$program_activities_funding_this_award)
table(OFA_combo_df$cfda_title)
nrow(OFA_combo_df)

# Evaluate all the methods applied to OHS. Intentionally in alphabetical order. 
OFA_list <- list(OFA_cfda_df, OFA_combo_df, OFA_funding_office_df,
                 OFA_pccodes_df, OFA_prog_acts_df)
OFA_list
names(OFA_list) # expect it to be NULL initially

OFA_objects_names <- ls()[grepl("^OFA.*df$", ls())] %>% sort()
OFA_objects_names 
names(OFA_list) <- OFA_objects_names
names(OFA_list)

# Output a summary table of methods applied to OFA 
OFA_combined_summary <- do.call(rbind, lapply(OFA_list, summarize_office_methods))
OFA_combined_summary

OFA_combined_summary$office <- 'OFA'
OFA_combined_summary$meth <- names(OFA_list)
OFA_combined_summary 

# Output our 3 summary tables (OHS, OFVPS, )
#-------------------------------------------------------------------------------
output_df <- rbind(OHS_combined_summary, 
                   OFVPS_combined_summary,
                   OFA_combined_summary)
output_df

xlsx_file <- paste0(OUT_FOLDER, '/ACF-offices-summary-table.xlsx')
write.xlsx(x = output_df, file = xlsx_file)
