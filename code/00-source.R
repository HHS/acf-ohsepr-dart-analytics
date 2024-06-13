#### 1) pkgs, env settings, & global vars
#-------------------------------------------------------------------------------
PKGS <- c('tidyverse', 'curl', 'digest', 
          'rvest', 'stringr', 'arrow',
           'RecordLinkage', 'stringdist',
          'RSelenium', 'rJava', 'httr',
          'binman', 'openxlsx')
lapply(PKGS, library, character.only = TRUE )
options(scipen = 99999, timeout = 90000)

# URL to update monthly:
GRANTS_URL <- "https://files.usaspending.gov/award_data_archive/FY2024_All_Assistance_Full_20240608.zip"

### GLOBAL DIRS
# ! update this to getwd() afterwards 
ROOT_DIR <- "C:/Users/Bela Reeves/Documents/@Raft - 2023 and after/@task-3"
setwd(ROOT_DIR)
CODE_DIR <- paste0(ROOT_DIR,"/code")
INPUT_DIR <- paste0(ROOT_DIR,"/inputs")
OUT_DIR <- paste0(ROOT_DIR, "/outputs")

# source requirements file 
REQ_source <- paste0(CODE_DIR, "/00-source-funcs.R")
file.exists(REQ_source)
# source(REQ_source)=======
  
### GLOBAL CONSTANTS 
# GETTING CURRENT YEAR, MONTH & DAY
CURR_DATE <- format(Sys.Date())
CURR_YEAR <- format(Sys.Date(), "%Y") %>% as.numeric()
CURR_MONTH <- substr(CURR_DATE, 6, 7)
CURR_DAY <- substr(CURR_DATE, 9, 10)
SYS_TIME <- as.character(Sys.time())
SYS_TIME

# logging code runs and managing output 
OUT_TIMESTAMP <- str_replace_all(SYS_TIME, ":", "")

OUT_FOLDER <- paste0(OUT_DIR, "/", OUT_TIMESTAMP)
OUT_FOLDER

if (dir.exists(OUT_FOLDER) == FALSE) {
  dir.create(OUT_FOLDER)
}

# GETTING RELEVANT FISCAL YEARS 
FY_YEARS <- (CURR_YEAR -5):CURR_YEAR
FY_YEARS
MIN_YEAR <- CURR_YEAR - 5
MIN_YEAR

#### 2) SOURCE FILES BY SUB-MODULE
#####~~~~~#####~~~~~#####~~~~~#####~~~~~#####~~~~~#####~~~~~#####~~~~~#####~~~~~

## 1) USA Spending Grants scripts 
USASPENDING_source_funcs <- paste0(CODE_DIR, "/01-usa-spending-grants-data-funcs.R")

USASPENDING_source <- paste0(CODE_DIR, "/01-usa-spending-grants-data.R")

file.exists(USASPENDING_source)
# source(USASPENDING_source)

## 2a) ACF Program Mapping scripts 
ACF_source <- paste0(CODE_DIR, "/02-acf-program-map.R")
file.exists(ACF_source)
source(ACF_source)

## 1b) Head Start facilities from public source
HS_source <- paste0(CODE_DIR, "/02-headstart-facilities.R")
# source(HS_source)

## 3) USA Spending Grants - identifying offices 
OFFICES_source <- paste0(CODE_DIR, "/03-usa-spending-offices.R")
file.exists(OFFICES_source)
# source(OFFICES_source)

## 4) USA Spending Grants - identifying programs within offices 
PROGRAMS_source <- paste0(CODE_DIR, "/03-usa-spending-programs.R")
file.exists(PROGRAMS_source)
# source(PROGRAMS_source)

## 5) Tribal entities identification via fuzzy matching etc
TRIBE_source <- paste0(CODE_DIR, "/@tribal dba aka fka script.R")
file.exists(TRIBE_source)
# source(PROGRAMS_source)