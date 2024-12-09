#  OFA entity reconciliation script 
#-------------------------------------------------------------------------------
# Purpose: The purpose of this script is to  demonstrate a PoC for entity 
# reconciliation based off of OFA data. For OFA, we have 2  data sources:
# 1) OFA_cfda_df
# 2) 
# Head Start data, clean it up, create relevant new variables (facility_id) and 
# filter out irrelevant locations (locations associated with 
# expired grant numbers).

#-------------------------------------------------------------------------------



entity_ofa <- unique(OFA_cfda_df$recipient_name_raw)
# View(ofa_combo_df)
length(entity_ofa)
entity_ofa

head(OFA_cfda_df$recipient_name_raw)
head(entity_ofa)
unique(entity_ofa) %>% length()
dist_matrix <- stringdistmatrix(entity_ofa, entity_ofa, method = "lv")
rownames(dist_matrix) <- entity_ofa
colnames(dist_matrix) <- entity_ofa

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
simpl_ofa_combo_df <- OFA_cfda_df %>% 
  select(keeps) %>% 
  group_by(recipient_name_raw, recipient_uei) %>% 
  slice(1) %>% 
  ungroup()

dim(simpl_ofa_combo_df)
View(simpl_ofa_combo_df)


#  joining 
ofa_lev_df1 <- left_join(filtered_dist_df, simpl_ofa_combo_df, 
                         by = c('name1' = 'recipient_name_raw')) 
View(ofa_lev_df)
dim(ofa_lev_df)

name1_ofa <- ofa_lev_df1 %>% 
  group_by(name1) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-name2) %>% 
  rename(recipient_name_raw = name1)
dim(name1_ofa)
View(name1_ofa)    

# name2 same thing 
ofa_lev_df2 <- left_join(filtered_dist_df, simpl_ofa_combo_df, 
                         by = c('name2' = 'recipient_name_raw')) 
View(ofa_lev_df2)
dim(ofa_lev_df2)

name2_ofa <- ofa_lev_df2 %>% 
  group_by(name2) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-name1) %>% 
  rename(recipient_name_raw = name2)
dim(name2_ofa)
View(name2_ofa)

