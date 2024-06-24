#### USA Spending Grants Data 
#-------------------------------------------------------------------------------
# Purpose: The purpose of this file is to pull grants data from the USA Spending
# data archive link. This new month's data is then binded to the historical data.
# We then perform and export some data quality checks.
# Outputs: 
#  1) full_grants_df.feather: a feather file (small size format) containing the
# most recent grants data and the historical data (2019-2023)
#  2) 01-usa_spending_asserts.xlsx: an excel file summarizing the results of the 
# data quality checks 
#-------------------------------------------------------------------------------

## Source USA Spending function files
source(USASPENDING_source_funcs)
USASPENDING_dir <- paste0(INPUT_DIR, '/usa-spending-data/')
if (dir.exists(USASPENDING_dir) == FALSE) {
  dir.create(USASPENDING_dir)
}

# Specify where usa-spending data quality checks will live 
USASPENDING_asserts <- paste0(OUT_FOLDER, '/usa-spending-asserts')
USASPENDING_asserts
if (dir.exists(USASPENDING_asserts) == FALSE) {
  dir.create(USASPENDING_asserts)
}

## Pull in the most recent data from USA Spending. 
most_recent_df <- get_grants_data(GRANTS_URL) 

most_recent_df <- most_recent_df %>% 
  #keeping only distinct transactional obs; this variable is dependable/accurate
  distinct(assistance_transaction_unique_key, .keep_all = T) %>% 
  rename(obligated_amount_from_COVID.19_supplementals_for_overall_award = `obligated_amount_from_COVID-19_supplementals_for_overall_award`,
         outlayed_amount_from_COVID.19_supplementals_for_overall_award = `outlayed_amount_from_COVID-19_supplementals_for_overall_award`) %>% 
  as.data.frame()

# Covid-related variables sometimes contain hyphens and sometimes contains 
# periods ("-' vs "."). In the hyphen case, they need to be renamed. 

most_recent_df <- covid_rename(most_recent_df)

## pulling in historical USA Spending Data - 5 year look-up. 
# The USA Spending's historical grant data's URLS exhibit inconsistent behavior,
# e.g., you pull from FY2021_All_Assistance_Full_20240516.zip and it will contain
# 2020, 2021 and 2024 data; and you pull from FY2020_All_Assistance_Full_20240516.zip 
# and it will pull 2020, 2021 and 2024 data. This inconsistency is 
# very computationally expensive, best to pull an assembled hist data set instead.

# ! you will need to update this file after the end of the year
hist_file <- paste0(USASPENDING_dir, 'hist-data/grants_2019-2023.feather')
hist_df <- read_feather(hist_file) %>% 
  distinct(assistance_transaction_unique_key, .keep_all = T) %>% 
  select(-one_of('X')) %>% 
  as.data.frame() 

## updating the overlap b/w the historical records and the new records, adding
# all net new records 
full_grants_df <- hist_df %>% 
  filter(!assistance_transaction_unique_key %in% 
           most_recent_df$assistance_transaction_unique_key) %>% 
  rbind(most_recent_df)
dim(full_grants_df) 

# calling write_full_grants function: function that exports the new hist data
write_full_grants(df = full_grants_df, URL = GRANTS_URL)

## Data validation checks -----------------------------------------------------

## assert_full_grants_data_check: a function to validate that rbind of new 
# observations was successful the expected and notable b/w the hist df and new 
# df assistance_transaction_unique_key overlap is an increased in the 
# `total_obligated_amount` var
assert_full_grants_result <- assert_full_grants_data_check(hist_df, 
                                                           most_recent_df, 
                                                           full_grants_df) %>% 
  as.data.frame()
assert_full_grants_result

## assert_action_vs_total_obligation: a function to validate that the sum of 
# federal_action_obligation === total_obligated_amount
asserts_obligations_df <- assert_action_vs_total_obligation(df = full_grants_df, 
                                                            CURR_DATE)

## assert_funding_office: a function to validate that the funding offices are 
# consistent across each grant's lifetime
funding_offices_df <- assert_funding_office(df = full_grants_df) %>% 
  arrange(desc(unique_count))

##export_usaspending_checks: function to export all checks to a single xlsx
export_usaspending_checks(assert_full_grants_result,
                          asserts_obligations_df,
                          funding_offices_df,
                          USASPENDING_asserts)
