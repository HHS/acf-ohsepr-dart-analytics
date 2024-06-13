#  Head Start locations script 
#-------------------------------------------------------------------------------
# Purpose: The purpose of this script is to pull in the publicly available 
# Head Start data, clean it up, create relevant new variables (facility_id) and 
# filter out irrelevant locations (locations associated with 
# expired grant numbers).

#-------------------------------------------------------------------------------

# URL containing the HS Service Locations 
HS_URL <- "https://eclkc.ohs.acf.hhs.gov/sites/default/files/locatordata/HS_Service_Locations.csv"

# Source HS Service Locations functions 
HS_source_funcs <- paste0(CODE_DIR, "/02-headstart-facilities-funcs.R")
source(HS_source_funcs)

# Specify the HS directory
HS_dir <- paste0(INPUT_DIR, '/head-start-data/')
if (dir.exists(HS_dir) == FALSE) {
  dir.create(HS_dir)
}

# Read in the raw data, keep only non-dupes, rename `grant_Number` 
raw_hs_df <- read.csv(HS_URL) %>% 
  distinct() %>% 
  as.data.frame() %>% 
  rename(grant_number = grant_Number)
dim(raw_hs_df)

# Expand abbreviations in the address_line_one column
expanded_hs_df <- expand_abbreviations(raw_hs_df, "address_line_one")

# Lower all predetermined character cols 
headstart_df <- head_start_to_lower(df = raw_hs_df)

# Create facility_id and facility_hash vars 
hash_headstart <- create_facility_hash(headstart_df)

# Filter data to only grants whose current performance data is relevant, i.e.
# remove facilities associated with grants that have expired 
relevant_FAINs <- full_grants_df %>% 
  filter(period_of_performance_current_end_date > CURR_DATE) %>% 
  pull(unique(award_id_fain))
length(relevant_FAINs)

# Filter to only relevant FAINs
cleaned_headstart <- hash_headstart %>% 
  filter(grant_number %in% relevant_FAINs)
dim(out_head_start_df)
dim(hash_headstart)

# Remove irrelevant dfs
rm(raw_hs_df, expanded_hs_df, headstart_df, hash_headstart)

# Output cleaned up Head Start facilities
write_head_start_locations()


## Data validation checks -----------------------------------------------------

# length unique HS rows vs length unique facility_hash
length(unique(cleaned_headstart$grant_number))
length(unique(raw_hs_df$grant_number))

length(unique(cleaned_headstart$facility_hash))
unique_output_count <- cleaned_headstart %>% 
  distinct() %>% 
  nrow()
unique_output_count
