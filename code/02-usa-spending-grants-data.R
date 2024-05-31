#### USA Spending Grants Data 
#####~~~~~#####~~~~~#####~~~~~#####~~~~~#####~~~~~#####~~~~~#####~~~~~#####~~~~~

# create fiscal year strings 




## base, stable archival data link 
BASE_URL <- "https://www.usaspending.gov/download_center/award_data_archive"

# possibly correct links 
good <- "https://files.usaspending.gov/award_data_archive/FY2024_All_Assistance_Full_20240516.zip"
is_link_valid(good)
zip_suffix <- paste(sep = "", "FY", CURR_YEAR, "_All_Assistance_Full_", 
                    CURR_YEAR, CURR_MONTH:CURR_MONTH-1, CURR_DAY, ".zip")
zip_suffix

if (CURR_DATE < )
POSS_URLS <- paste0(sep = '/', BASE_URL, zip_suffix)
POSS_URLS
#---
link_node <- page %>%
  html_nodes("div#app main")

link_node
link_node <- webpage %>%
  html_nodes("div#app main#main-content.usda-page_container div.award-data-archive-table table tbody td.row.odd a")

## unstable download link
GRANT_URLS <- paste0("https://files.usaspending.gov/award_data_archive/FY", FY_YEARS, "_All_Assistance_Full_2024", "0408.zip")

## GET_GRANT_URL(): function to get this month's URL for usa spending data
get_grant_url <- function(BASE_URL) {

  browser()
  return(VER_GRANT_URL)

}
# download_link <- page %>%
#   html_nodes("a[href^='https://files.usaspending.gov/award_data_archive/FY2024_All_Assistance_Full']") %>%
#   html_attr("href")
# print(download_link)
# 
# print(page)
# 
# writeLines(as.character(page), "page_content.html")
