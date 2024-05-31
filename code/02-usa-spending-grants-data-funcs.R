#### USA Spending - Grants Data 
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

##### is_link_valid(): function to test if a link is valid 
is_link_valid <- function(url) {
  response <- HEAD(url)
  status_code <- status_code(response)
  
  # print(status_code)
  if (status_code == 200) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#### get_grant_data(): function to get the grants data from the grant linnk 
get_grant_data <- function(url) {
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
    
    
    data <- data_frame()
    # Check if there are CSV files
    if (length(csv_files) > 0) {
      # Read the data from the CSV file
      print('printing the csv file names:')
      print(csv_files)
      files_num <- length(csv_files)
      data <- read_csv(csv_files[1], show_col_types = FALSE) %>% 
        filter(awarding_sub_agency_code == '7590')
      # Add the year as a column 
      data$year <- str_extract(url[1], "\\d{4}")
      
      if (length(csv_files) > 1) {
        
        for (file in csv_files[2:length(csv_files)]) {
          print('printing the specific file thats going to be processed:')
          print(file) 
          data2 <- read_csv(file, show_col_types = FALSE) %>% 
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
}
