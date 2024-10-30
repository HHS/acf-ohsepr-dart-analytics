#  ofvps entity reconciliation script 
#-------------------------------------------------------------------------------
# Purpose: The purpose of this script is to  demonstrate a PoC for entity 
# reconciliation based off of ofvps data. For ofvps, we have 2  data sources:
# 1) ofvps_cfda_df
# 2) 
# Head Start data, clean it up, create relevant new variables (facility_id) and 
# filter out irrelevant locations (locations associated with 
# expired grant numbers).

#-------------------------------------------------------------------------------


entity_ofvps <- unique(OFVPS_combo_df$recipient_name_raw)
# View(ofvps_combo_df)
length(entity_ofvps)
entity_ofvps

head(OFVPS_combo_df$recipient_name_raw)
head(entity_ofvps)
unique(entity_ofvps) %>% length()
dist_matrix <- stringdistmatrix(entity_ofvps, entity_ofvps, method = "lv")
rownames(dist_matrix) <- entity_ofvps
colnames(dist_matrix) <- entity_ofvps

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
simpl_ofvps_combo_df <- OFVPS_combo_df %>% 
  select(keeps) %>% 
  group_by(recipient_name_raw, recipient_uei) %>% 
  slice(1) %>% 
  ungroup()

dim(simpl_ofvps_combo_df)
# View(simpl_ofvps_combo_df)


#  joining 
ofvps_lev_df1 <- left_join(filtered_dist_df, simpl_ofvps_combo_df, 
                         by = c('name1' = 'recipient_name_raw')) 
# View(ofvps_lev_df)

name1_ofvps <- ofvps_lev_df1 %>% 
  group_by(name1) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-name2) %>% 
  rename(recipient_name_raw = name1)
dim(name1_ofvps)
# View(name1_ofvps)    

# name2 same thing 
ofvps_lev_df2 <- left_join(filtered_dist_df, simpl_ofvps_combo_df, 
                         by = c('name2' = 'recipient_name_raw')) 
# View(ofvps_lev_df2)
dim(ofvps_lev_df2)

name2_ofvps <- ofvps_lev_df2 %>% 
  group_by(name2) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-name1) %>% 
  rename(recipient_name_raw = name2)
dim(name2_ofvps)
# View(name2_ofvps)

