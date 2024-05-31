# Load required packages
library(rvest)

# Define the URL
BASE_URL <- "https://www.usaspending.gov/download_center/award_data_archive"

# Read the web page
page <- read_html(BASE_URL)

# Construct an XPath selector
link_node <- page %>%
  html_nodes(xpath = "//div[@id='app']//main[@id='main-content' and contains(@class, 'usda-page_container')]//div[contains(@class, 'award-data-archive-table')]//table//tbody//td[contains(@class, 'row odd')]//a")

# Extract the href attribute from the <a> tag
link <- link_node %>% html_attr("href")

# Print the extracted link
print(link)


# -----------
# Navigate to the web page
install.packages("RSelenium")
library(RSelenium)

file.edit("~/.Renviron")
# JAVA_HOME="C:/Program Files/Java/jdk-22"

# Set JAVA_HOME environment variable
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk-22")
Sys.setenv(PATH = paste(Sys.getenv("JAVA_HOME"), "bin", sep = "/"))

# 
rD <- rsDriver(browser = "chrome", port = 4545L)
remDr <- rD$client
remDr$navigate("https://www.usaspending.gov/download_center/award_data_archive")

# Allow time for the page to load
Sys.sleep(5)  # Adjust sleep time as necessary

# Extract page source after JavaScript execution
page_source <- remDr$getPageSource()[[1]]

# Use rvest on the retrieved page source
page <- read_html(page_source)

# Construct the selector again
link_node <- page %>%
  html_nodes("div#app main#main-content.usda-page_container div.award-data-archive-table table tbody td.row.odd a")

# Extract the href attribute from the <a> tag
link <- link_node %>% html_attr("href")

# Print the extracted link
print(link)

# Close the browser
remDr$close()
rD$server$stop()


# -----

library(httr)

# is_link_valid(): function to test if a link is valid 
is_link_valid <- function(url) {
  response <- HEAD(url)
  status_code <- status_code(response)
  
  # print(status_code)
  if (status_code == 200) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Example usage
url <- "https://www.usaspending.gov/download_center/award_data_archive"
is_valid <- check_link(url)

links <- "https://files.usaspending.gov/award_data_archive/FY2024_All_Contracts_Full_20240517.zip"
check_link(other)
if (is_valid) {
  print("The link is valid.")
} else {
  print("The link is not valid.")
}
