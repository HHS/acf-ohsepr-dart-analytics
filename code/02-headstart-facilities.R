#  Head Start locations script 
#-------------------------------------------------------------------------------
# Purpose: The purpose of this script is to pull in the publically available 
# Head Start data, clean it up, create relevant new variables (facility_id) and 
# filter out irrelevant locations (locatiosn associated with 
# expired grant numbers).

#-------------------------------------------------------------------------------

# URL containing the HS Service Locations 
HS_URL <- "https://eclkc.ohs.acf.hhs.gov/sites/default/files/locatordata/HS_Service_Locations.csv"

# Source HS Service Locations functions 
source(HS_source_funcs)

# Read in the raw data, remove duplicates
raw_hs_df <- read.csv(HS_URL) %>% 
  distinct() %>% 
  as.data.frame() %>% 
  rename(grant_number = grant_Number)
dim(raw_hs_df)

# Expand abbreviations in the address_line_one column
expanded_hs_df <- expand_abbreviations(raw_hs_df, "address_line_one")

# Create the Facility ID
# ['address_line_one', 'zip_five', 'state_code', 'location_name',
  # 'program_name', 'grant_number']
KEEPS <- c('add')
create_facility_id <- function() {
  
  
}
# Filter data to only grants whose current performance data is relevant, i.e.
# remove facilities associated with grants that have expired 

# functions to make:
# [x] expand all abbreviated street names (St, Blvd, etc)
# [] create Facility ID
# [] filter HS facilities to 
# [] asserts: diff b/w unique HS facilities in raw data vs 

## Data validation checks -----------------------------------------------------
