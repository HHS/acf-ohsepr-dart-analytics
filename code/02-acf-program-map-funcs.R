#### ACF program mapping functions 
#-------------------------------------------------------------------------------
# Purpose: This file stores the ACF Program Mapping's relevant functions. 
#-------------------------------------------------------------------------------

#### rename columns function 
rename_acf_program_map <- function(input_pm) {
  
  acf_new_colnames <- colnames(input_pm) %>% tolower()
  acf_new_colnames <- gsub(" ", "_", acf_new_colnames)
  acf_new_colnames
  colnames(input_pm) <- acf_new_colnames
  
  cat('The ACF Program Mapping columns have been successfully renamed.')
  return(input_pm)
}

####


####

####
