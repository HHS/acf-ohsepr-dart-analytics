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

# Specify output file name and out directory 
ACT_OUT_NAME <- '/acf_program_mapping.feather'
OUT_DIR_ACF_MAPPING <- paste0(OUT_FOLDER, ACT_OUT_NAME)
source(ACF_funcs, echo = TRUE, verbose = TRUE)

# Read in ACF program mapping data source(s)

# !! change this: 
acf_raw <- readxl::read_xlsx("C:/Users/Bela Reeves/Documents/@Raft - 2023 and after/@task-3/inputs/acf-program-mapping/ACF Program List Export - OFVPS - 2024-04.xlsx")

# Rename the columns using the rename_acf_program_map() function 
acf_program_map <- rename_acf_program_map(acf_raw)
colnames(acf_program_map)

## Showing the distribution of program codes by Office
table(acf_program_map$program) %>% as.data.frame()


# Output the ACF Program Map 
write_feather(acf_program_map, OUT_DIR_ACF_MAPPING)

