#  OHS entity reconciliation script 
#-------------------------------------------------------------------------------
# Purpose: The purpose of this script is to  demonstrate a PoC for entity 
# reconciliation based off of OHS data. For OHS, we have 2  data sources:
# 1) The OHS Facilities data, i.e. `cleaned_headstart` in memory, and 
# 2) 
# Head Start data, clean it up, create relevant new variables (facility_id) and 
# filter out irrelevant locations (locations associated with 
# expired grant numbers).

#-------------------------------------------------------------------------------

# Show the count of facility entries vs the count of unique facility hashes
nrow(cleaned_headstart) - length(unique(cleaned_headstart$facility_hash))

# We use the combo method df going forward; # of unique OHS grants
length(unique(OHS_combo_df$award_id_fain))

# Assess facility coverage vs our OHS grants population
# One grant can fund multiple facilities, so this is not necessarily problematic.

# Filter to only active grants, since locations 
active_OHS_df <- OHS_combo_df %>% 
  filter(period_of_performance_current_end_date > CURR_DATE)
length(unique(active_OHS_df$award_id_fain))

# Join the facility_hash and facility_id vars to our OHS df to assess coverage
# Expect a many-to-many match due to one grant funding multiple programs 
OHS_combo_hash_df <- left_join(x = OHS_combo_df, 
                               y = cleaned_headstart[, c('grant_number', 
                                                         'facility_id', 
                                                         'facility_hash')], 
                               by = c('award_id_fain' = 'grant_number'))

# Show many-to-many coverage - roughly 10% of all observations are missing 
sum(is.na(OHS_combo_hash_df$facility_hash))/nrow(OHS_combo_hash_df)

# no_matches_df: Dataframe with grants w/o associated facility data 
no_matches_df <- OHS_combo_hash_df %>% 
  filter(is.na(facility_hash)) 

# Show absolute grant coverage - roughly 8.6% 
length(unique(no_matches_df$award_id_fain))/nrow(OHS_combo_df)
View(no_matches_df)
OHS_combo_hash_df %>% filter(is.na)

# Show the dist of facilities by grant - does one have multiple facilities?
# We expect yes 

# to do:

# get_shared_facilities():
# gen_shared_facilities_id():
# 


# Show the distribution of UEIs per facility - 
KEEPS <- c('award_id_fain', 'facility_hash', 'facility_id', 
           'recipient_uei', 'recipient_parent_uei')
summ_facilities_df <- OHS_combo_hash_df[, KEEPS] %>% 
  group_by(facility_hash) %>% 
  # this UEI unique is a new acf_id_hash 
  mutate(uei_unique = dense_rank(recipient_uei)) %>% 
  ungroup() %>% 
  group_by(facility_hash, uei_unique) %>% 
  slice(1)

summ_facilities_df %>% filter(!is.na(facility_hash)) %>% 
  arrange(facility_hash) %>% View()

string1 <- 'YWCA OF GREATER BATON ROUGE' 

string2 <- 'CITY OF NEW YORK BOARD OF EDUCATION'
stringdist::stringdist(string1, string2, method = 'lv') 

entity_ohs <- unique(OHS_combo_df$recipient_name_raw)
unique(OHS_combo_df)
# View(OHS_combo_df)
length(entity_ohs)
entity_ohs

head(OHS_combo_df$recipient_name_raw)
head(entity_ohs)
unique(entity_ohs)
dist_matrix <- stringdistmatrix(entity_ohs, entity_ohs, method = "lv")
rownames(dist_matrix) <- entity_ohs
colnames(dist_matrix) <- entity_ohs

dist_matrix %>% View()

# Convert distance matrix to long format
dist_df <- as.data.frame(as.table(as.matrix(dist_matrix)))
View(dist_df)
colnames(dist_df) <- c("name1", "name2", "distance")

# Filter pairs with a distance of 9 or less, but not 0
filtered_dist_df <- dist_df %>% 
  filter(distance <= 7 & distance > 0)

# Create a new ID variable for these pairs
filtered_dist_df <- filtered_dist_df %>%
  group_by(name1) %>%
  mutate(new_id = cur_group_id()) %>%
  ungroup() %>% 
  mutate(hash_id = paste(name1, name2, distance, sep = "_")) %>% 
  mutate(lev_hash = sapply(hash_id, digest),
         method = 'levenshtein') %>% 
  select(-new_id)
dim(filtered_dist_df)

# left join to create new reverse df
keeps <- c('recipient_name_raw', 'recipient_uei')
simpl_ohs_combo_df <- OHS_combo_df %>% 
  select(keeps) %>% 
  group_by(recipiet_name_raw, recipient_uei) %>% 
  slice(1) %>% 
  ungroup()
dim(simpl_ohs_combo_df)
View(simpl_ohs_combo_df)
#  joining 
ohs_lev_df1 <- left_join(filtered_dist_df, simpl_ohs_combo_df, 
                        by = c('name1' = 'recipient_name_raw')) 
View(ohs_lev_df)
dim(ohs_lev_df)

name1_ohs <- ohs_lev_df1 %>% 
  group_by(name1) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-name2) %>% 
  rename(recipient_name_raw = name1)
dim(name1_ohs)
View(name1_ohs)    

# name2 same thing 
ohs_lev_df2 <- left_join(filtered_dist_df, simpl_ohs_combo_df, 
                         by = c('name2' = 'recipient_name_raw')) 
View(ohs_lev_df2)
dim(ohs_lev_df2)

name2_ohs <- ohs_lev_df2 %>% 
  group_by(name2) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-name1) %>% 
  rename(recipient_name_raw = name2)
dim(name2_ohs)
View(name2_ohs)

# Create a lookup table to map each recipient name to the new ID
lookup_df <- filtered_dist_df %>%
  select(name1, new_id) %>%
  distinct() %>%
  rename(recipient_name_raw = name1)

head(lookup_df)

#  Method: Hash filt
id_OHS_hash <- OHS_combo_hash_df %>% filter(!is.na(facility_hash)) %>% 
  unique() %>% 
  mutate(method = 'facility_hash')

View(id_OHS_hash)

# Join the new ID variable to the initial data frame
OHS_combo_df <- OHS_combo_df %>%
  left_join(lookup_df, by = "recipient_name_raw")

KEEPS <- c('assigned_office', 'office_confidence', 
           'facility_hash', 'method', 'recipient_uei',
           'recipient_duns', 'recipient_name_raw')
ohs_list_dedup <- id_OHS_hash %>% 
  select(KEEPS) %>% 
  arrange(facility_hash) %>% 
  unique() %>% 
  group_by(facility_hash) %>% 
  mutate(n = row_number())

View(ohs_list_dedup)
dim(ohs_list_dedup)

#  would need to filter only to count(facility_hash) > 1 to see interesting records
ohs_list_dedup <-ohs_list_dedup %>% 
  select(assigned_office, facility_hash, method, recipient_uei, recipient_name_raw) %>% 
  distinct() %>% 
  group_by(assigned_office, facility_hash, method, recipient_uei) %>% 
  slice(1) %>% 
  ungroup()

# KQN2BSLHJRA9


View(ohs_list_dedup)

write.xlsx(x = ohs_list_dedup, file = "C:/Users/Bela Reeves/Documents/@Raft - 2023 and after/@task-3/outputs/2024-06-25 092802/ACF_dedupe_df.xlsx")


View(ohs_list_dedup)
# If there are any names that didn't get a new_id (because they had no matches),
# assign them a unique ID
OHS_combo_df <- OHS_combo_df %>%
  mutate(new_id = if_else(is.na(new_id), row_number() + max(lookup_df$new_id, na.rm = TRUE), new_id))



dim(id_OHS_hash)
View(id_OHS_hash)
dim(OHS_combo_hash_df)
