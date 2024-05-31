####  get tribal function output including pka and aka 
tribal_doc <- readxl::read_xlsx("C:/Users/Bela Reeves/Documents/@Raft - 2023 and after/@task-3/outputs/2024-05-22 125643/document number 2024-00109.xlsx")
View(tribal_doc)
dim(tribal_doc) #13  5

### BLUF: able to successfully fuzzy match on NEW NAME - not PKA or AKA.
## we can use this info to keep track of aliases. 
## therefore: investing in a tribal federal registries AKA/PKA dataset is worth it 


#next steps:
  # expand tribal doc set to include 100+ obs, then use the Levenstein method 
# usa_spending_df keeps
USA_SPENDING_KEEPS <- c(colnames(usa_spending_df)[grepl('recipient_name|duns|uei|fain|address',colnames(usa_spending_df), 
                                           ignore.case = TRUE)],
           'award_id_fain')
USA_SPENDING_KEEPS

# to lower - will roll this into the data prep function in 02
usa_spending_df$recipient_name_raw <- tolower(usa_spending_df$recipient_name_raw)
usa_spending_df$recipient_name <- tolower(usa_spending_df$recipient_name)

tribal_doc$new_name <- tolower(tribal_doc$new_name)
tribal_doc$pka_name <- tolower(tribal_doc$pka_name)

# what do NA names look like within the (preliminary) ANA pull?
ana_pull <- prime_get_ANA(usa_spending_df)
table(ana_pull$recipient_name_raw) %>% View()

#### fuzzy join
##########################################

### 1) lv method - pka 
lv_dist_pka <- stringdist_join(tribal_doc, usa_spending_df[USA_SPENDING_KEEPS], 
                           by = c('pka_name' = 'recipient_name_raw'),
                           mode = 'left',
                           method = 'lv',
                           max_dist = 1)
sum(is.na(lv_dist_pka$recipient_name_raw)) # all na 
View(lv_dist_pka)
### lv method - aka
lv_dist_newname <- stringdist_join(tribal_doc, usa_spending_df[USA_SPENDING_KEEPS], 
                               by = c('new_name' = 'recipient_name_raw'),
                               mode = 'left',
                               method = 'lv',
                               max_dist = 1)
sum(is.na(lv_dist_newname$recipient_name_raw)) # 10 NA

View(lv_dist_newname)
dim(lv_dist_newname) #28 15

### 2) LONGEST COMMON SUBSTRING DISTANCE method - pka 
## BLUF: a lot of false positives 
lcs_dist_pka <- stringdist_join(tribal_doc, usa_spending_df[USA_SPENDING_KEEPS], 
                               by = c('pka_name' = 'recipient_name_raw'),
                               mode = 'left',
                               method = 'lcs',
                               max_dist = 1)
sum(is.na(lcs_dist_pka$recipient_name_raw)) #  13
View(lcs_dist_pka)
dim(lcs_dist_pka) #13 15

### LONGEST COMMON SUBSTRING DISTANCE  DISTANCE method - new name 
lcs_dist_newname <- stringdist_join(tribal_doc, usa_spending_df[USA_SPENDING_KEEPS], 
                                   by = c('new_name' = 'recipient_name_raw'),
                                   mode = 'left',
                                   method = 'lcs',
                                   max_dist = 2)
sum(is.na(lcs_dist_newname$recipient_name_raw)) # 10

View(lcs_dist_newname)
dim(lcs_dist_newname) #28 15

### 3) JACCARD DISTANCE method - pka 
## BLUF: a lot of false positives 
jacc_dist_pka <- stringdist_join(tribal_doc, usa_spending_df[USA_SPENDING_KEEPS], 
                               by = c('pka_name' = 'recipient_name_raw'),
                               mode = 'left',
                               method = 'jaccard',
                               max_dist = 2)
sum(is.na(jacc_dist_pka$recipient_name_raw)) # none NA 
View(jacc_dist_pka)
dim(jacc_dist_pka) # 2278887      15 - insane; too many FPs 

### LONGEST COMMON SUBSTRING DISTANCE method - aka
jacc_dist_newname <- stringdist_join(tribal_doc, usa_spending_df[USA_SPENDING_KEEPS], 
                                     by = c('new_name' = 'recipient_name_raw'),
                                     mode = 'left',
                                     method = 'jaccard',
                                     max_dist = 1)
sum(is.na(jacc_dist_newname$recipient_name_raw)) # none NA

View(jacc_dist_newname)
dim(jacc_dist_newname) #2278887      15


### 4) JACCARD DISTANCE method - pka 
## BLUF: a lot of false positives; 
# makes sense term frequency based methods are not suited for low density NLP
jacc_dist_pka <- stringdist_join(tribal_doc, usa_spending_df[USA_SPENDING_KEEPS], 
                                 by = c('pka_name' = 'recipient_name_raw'),
                                 mode = 'left',
                                 method = 'jaccard',
                                 max_dist = 2)
sum(is.na(jacc_dist_pka$recipient_name_raw)) # none NA 
View(jacc_dist_pka)
dim(jacc_dist_pka) # 2278887      15 - insane; too many FPs 

### JACCARD DISTANCE method - aka
jacc_dist_newname <- stringdist_join(tribal_doc, usa_spending_df[USA_SPENDING_KEEPS], 
                                     by = c('new_name' = 'recipient_name_raw'),
                                     mode = 'left',
                                     method = 'jaccard',
                                     max_dist = 1)
sum(is.na(jacc_dist_newname$recipient_name_raw)) # none NA

View(jacc_dist_newname)
dim(jacc_dist_newname) #2278887      15

### 5) soundex DISTANCE method - pka 
soundex_dist_pka <- stringdist_join(tribal_doc, usa_spending_df[USA_SPENDING_KEEPS], 
                                 by = c('pka_name' = 'recipient_name_raw'),
                                 mode = 'left',
                                 method = 'soundex')
sum(is.na(soundex_dist_pka$recipient_name_raw)) # 4
View(soundex_dist_pka)
dim(soundex_dist_pka) # 1202   15 - too high 

### soundex DISTANCE method - aka
soundex_dist_newname <- stringdist_join(tribal_doc, usa_spending_df[USA_SPENDING_KEEPS], 
                                     by = c('new_name' = 'recipient_name_raw'),
                                     mode = 'left',
                                     method = 'soundex')
sum(is.na(soundex_dist_newname$recipient_name_raw)) # 4

View(soundex_dist_newname)
dim(soundex_dist_newname) # 2148   15


### 6) Jaro-Winkler DISTANCE method - pka 
jw_dist_pka <- stringdist_join(tribal_doc, usa_spending_df[USA_SPENDING_KEEPS], 
                                    by = c('pka_name' = 'recipient_name_raw'),
                                    mode = 'left',
                                    method = 'jw')
sum(is.na(jw_dist_pka$recipient_name_raw)) # 0
View(jw_dist_pka)
dim(jw_dist_pka) # 2278887      15 - too high 

### Jaro-Winkler DISTANCE method - new name
jw_dist_newname <- stringdist_join(tribal_doc, usa_spending_df[USA_SPENDING_KEEPS], 
                                        by = c('new_name' = 'recipient_name_raw'),
                                        mode = 'left',
                                        method = 'jw')
sum(is.na(jw_dist_newname$recipient_name_raw)) # 4

View(jw_dist_newname)
dim(jw_dist_newname) # 2278887      15
# -------------------------------------------
##  recordlinkage

## stringdist
