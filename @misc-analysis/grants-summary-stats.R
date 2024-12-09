#### USA Spending Data: Summary Stats
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

library(feather)
library(arrow)
library(tidyverse)
# input_file <- "C:/Users/Bela Reeves/Documents/@Raft - 2023 and after/@task-3/inputs/usa-spending-data/full_grants_data_202407.feather"
input_file <- "C:/Users/Bela Reeves/Documents/@Raft - 2023 and after/@task-3/inputs/usa-spending-data/full_grants_data_202406.feather"
input_df <- read_feather(input_file)
# distribution of facilities per grantees

# address standardization to prevent false groupings 
summ_df <- input_df %>% 
  mutate(facility_id = paste0(recipient_address_line_1, recipient_address_line_2, 
                recipient_city_code, recipient_state_code,
                recipient_country_code))

head(summ_df$facility_id)  

all_acf <- summ_df %>% 
  group_by(recipient_uei) %>% 
  summarise(num_facilities = n()) %>% 
  summarise(
    mean = mean(num_facilities),
    median = median(num_facilities),
    sd = sd(num_facilities),
    min = min(num_facilities),
    max = max(num_facilities))
all_acf
