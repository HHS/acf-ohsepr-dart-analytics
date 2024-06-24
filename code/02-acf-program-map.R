#### ACF program mapping 
#-------------------------------------------------------------------------------
# Purpose: The purpose of this file is to pull in the ACF program mappings.
# There are numerous problems with this data: some grants and programs have been 
# reassigned # to different offices without documentation or metadata capturing 
# this, and  each office has its own regex format that must be targeted and this
# is not  documented.
#-------------------------------------------------------------------------------

# Source ACF mapping functions 
ACF_funcs <- paste0(CODE_DIR, "/02-acf-program-map-funcs.R")
source(ACF_funcs, echo = TRUE, verbose = TRUE)

# Specify output file name and out directory 
ACT_OUT_NAME <- 'acf_program_mapping.feather'
ACF_dir <- paste0(INPUT_DIR, '/acf-program-mapping/')
ACF_dir
if (dir.exists(ACF_dir) == FALSE) {
  dir.create(ACF_dir)
}
ACF_OUT_FILE <- paste0(ACF_dir, ACT_OUT_NAME)

# Read in ACF program mapping data source(s)
acf_raw <- readxl::read_xlsx(paste0(ACF_dir, 'ACF Program List Export - OFVPS - 2024-04.xlsx'))

# Rename the columns using the rename_acf_program_map() function 
acf_program_map <- rename_acf_program_map(acf_raw)
colnames(acf_program_map)

## Showing the distribution of program codes by Office
table(acf_program_map$program) %>% as.data.frame()

# Output the ACF Program Map 
write_feather(acf_program_map, ACF_OUT_FILE)
