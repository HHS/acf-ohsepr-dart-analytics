#### ACF program mapping 
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#### sourcing ACF mapping functions 
ACF_funcs <- paste0(CODE_DIR, "/01-acf-program-map-funcs.R")

# output file name
ACT_OUT_NAME <- '/acf_program_mapping.feather'

source(ACF_funcs, echo = TRUE, verbose = TRUE)

#### reading in ACF program mapping data source(s)

acf_raw <- readxl::read_xlsx("C:/Users/Bela Reeves/Documents/@Raft - 2023 and after/@task-3/inputs/acf-program-mapping/ACF Program List Export - OFVPS - 2024-04.xlsx")

#### renaming the columns 
acf_program_map <- rename_acf_program_map(acf_raw)
colnames(acf_program_map)

## distribution of program code counts by Office
table(acf_program_map$program) %>% as.data.frame()

OUT_DIR_ACF_MAPPING <- paste0(OUT_FOLDER, ACT_OUT_NAME)

write_feather(acf_program_map, OUT_DIR_ACF_MAPPING)

# old acf map 
# acf_old <- readxl::read_xlsx("C:/Users/Bela Reeves/Documents/@Raft - 2023 and after/@task-3/inputs/acf-program-mapping/ACF Program Map_20240104.xlsx")
# 
# acf_old_renamed <- rename_acf_program_map(acf_old)
# table(acf_old_renamed$program) %>% as.data.frame()
