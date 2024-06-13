#### USA Spending - Grants Data 
#-------------------------------------------------------------------------------
# Purpose: This script stores the USA Spending script's particular functions.
#-------------------------------------------------------------------------------

##### get_grants_data: function to get pull the most recent grants data from the
# usa spending grants data archive. 
get_grants_data <- function(url) {
  tryCatch({
    temp <- tempfile(fileext = ".zip")
    
    # Download the zip file
    download.file(url, temp, mode = "wb")
    
    # Check if the file exists
    if (file.exists(temp)) {
      # Specify the destination directory for extraction
      dest_dir <- tempdir()
      
      # Extract all files from the zip file
      unzip(temp, exdir = dest_dir)
      
      # Identify the CSV file within the extracted files
      csv_files <- list.files(dest_dir, pattern = "\\.csv$", full.names = TRUE)
      
      data <- data.frame()
      # Check if there are CSV files
      if (length(csv_files) > 0) {
        # Read the data from the CSV file
        print('printing the csv file names:')
        print(csv_files)
        files_num <- length(csv_files)
        data <- read.csv(csv_files[1]) %>% 
          filter(awarding_sub_agency_code == '7590')
        # Add the year as a column 
        data$year <- str_extract(url[1], "\\d{4}")
        
        if (length(csv_files) > 1) {
          for (file in csv_files[2:length(csv_files)]) {
            print('printing the specific file thats going to be processed:')
            print(file) 
            data2 <- read.csv(file) %>% 
              filter(awarding_sub_agency_code == '7590') 
            # Add the year as a column 
            data2$year <- str_extract(file, '\\d{4}')
            data <- rbind(data, data2)
          }
        }
        
        print(length(csv_files))
        
        # Remove the temporary file
        unlink(temp)
        
        return(data)
      } else {
        stop("No CSV files found in the extracted content.")
      }
    } else {
      stop("Downloaded file does not exist.")
    }
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })
}


##### write_full_grants: function to write the new full grants df 
write_full_grants <- function(df = full_grants_df, URL = GRANTS_URL) {
  browser()
  
  # Pull the 'YYYYMM' data from the most recent month's URL
  date <- substr(x = URL, start = 77, stop = 82)
  
  full_file_name <- paste0(USASPENDING_dir, 'full_grants_data_', date, 
                           '.feather')
  write_feather(x = full_grants_df, full_file_name)
  
  # Print a message to indicate success
  cat("'full_grants_df' has successfully exported.")
  
}


## Data validation checks -----------------------------------------------------

##### assert_full_grants_data_check: Function to validate that rbind
# of new observations was successful. The expected and notable dif b/w the 
# hist df and new df assistance_transaction_unique_key  overlap is an increased 
# in the `total_obligated_amount` var
assert_full_grants_data_check <- function(hist_df, 
                                          most_recent_df,
                                          full_grants_df) {
  test <- hist_df %>% 
    filter(!assistance_transaction_unique_key %in% most_recent_df$assistance_transaction_unique_key) %>% 
    nrow() + nrow(most_recent_df) == nrow(full_grants_df)
  
  if (!test) {
    stop("Data integrity check failed: The rows in full_grants_df do not match the expected count.")
  }
  
  return(TRUE)
}

##### assert_action_vs_total_obligation: function to validate that the sum of 
# federal_action_obligation === total_obligated_amount
assert_action_vs_total_obligation <- function(df = full_grants_df, CURR_DATE) {
  KEEPS <- c('award_id_fain', 'period_of_performance_current_end_date',
             'total_obligated_amount')
  # browser()
  # total_obligated_amount
  summ_df <- df %>% 
    filter(period_of_performance_current_end_date < CURR_DATE) %>% 
    group_by(award_id_fain) %>% 
    summarise(checksum = sum(federal_action_obligation)) 
  return(summ_df)
  # browser()
  total_ob_df <- df[, KEEPS] %>% 
    filter(period_of_performance_current_end_date < CURR_DATE) %>%
    group_by(award_id_fain) %>% 
    slice_head(n = 1)
  
  # 
  merged_df <- merge(summ_df, total_ob_df,
                     'award_id_fain') %>% 
    mutate(checksum_flag = checksum == total_obligated_amount)
  
  return(merged_df)
}



#### assert_funding_office: a function to validate that the funding offices are 
# consistent across each grant's lifetime
assert_funding_office <- function(df = full_grants_df) {
  
  summ_df <- full_grants_df %>% 
    group_by(award_id_fain) %>% 
    summarize(unique_values = list(unique(funding_office_name)),
              unique_count = n_distinct(funding_office_name))
  
  return(summ_df)
  
}

#### export_usaspending_checks: function to exporting all checks into a single 
# xlsx file 
export_usaspending_checks <- function(assert_full_grants_result,
                                      asserts_obligations_df,
                                      funding_offices_df,
                                      USASPENDING_asserts) { 
  # Create a new workbook
  wb <- createWorkbook()
  
  # Add each dataframe as a new sheet
  addWorksheet(wb, "assert_full_grants_result")
  writeData(wb, "assert_full_grants_result", assert_full_grants_result,
            na.string = "NA")
  
  addWorksheet(wb, "asserts_obligations_df")
  writeData(wb, "asserts_obligations_df", asserts_obligations_df,
            na.string = "NA")
  
  addWorksheet(wb, "funding_offices_df")
  writeData(wb, "funding_offices_df", funding_offices_df,
            na.string = "NA")
  
  # Save the workbook to an Excel file
  saveWorkbook(wb = wb, 
               file = paste0(USASPENDING_asserts, "/01-usa_spending_asserts.xlsx"), 
               overwrite = TRUE)
  
  # Print a message to indicate success
  cat("Excel file '02-usa_spending_asserts.xlsx' has been created with 3 sheets.")
}

export_usaspending_checks(assert_full_grants_result,
                          asserts_obligations_df,
                          funding_offices_df,
                          USASPENDING_asserts)

## Retired funcs ---------------------------------------------------------------

##### get possible URLs func:
get_poss_urls <- function(...) {
  THIS_MONTH <- paste0(CURR_YEAR, 
                       paste0(CURR_MONTH, paste0("0",CURR_DAY:1)))
  THIS_MONTH
  
  LAST_MONTH <- paste0(CURR_YEAR,
                       paste0(paste0(sep = "0", as.integer(CURR_MONTH)-1), 
                              paste0("0",1:31)))
  LAST_MONTH
  
  CON_MONTHS <- c(THIS_MONTH, LAST_MONTH)
  CON_MONTHS
  
  zip_suffix <- paste(sep = "", "FY", CURR_YEAR, "_All_Assistance_Full_", 
                      CON_MONTHS, ".zip")
  
  POSS_URLS <- paste0(BASE_URL, zip_suffix)
  return(POSS_URLS) } 

##### is_link_valid(): function to test if a link is valid 
## does not work for USA Spending data download center - invalid links return '200'
is_link_valid <- function(url) {
  response <- HEAD(url)
  status_code <- status_code(response)
  
  return(status_code)
  if (status_code == 200) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}