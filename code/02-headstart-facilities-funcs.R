#  Head Start locations functions 
#-------------------------------------------------------------------------------
# Purpose: This file stores all of the Head Start script's relevant functions.
#-------------------------------------------------------------------------------

#### expand_abbreviations: function to expand all street names out to achieve 
# address line standardization. 

abbreviations <- c(
  "St" = "Street",
  "Blvd" = "Boulevard",
  "Rd" = "Road",
  "Ave" = "Avenue",
  "Dr" = "Drive",
  "Ln" = "Lane",
  "Ct" = "Court",
  "Pl" = "Place",
  "Sq" = "Square",
  "Pkwy" = "Parkway",
  "Cir" = "Circle",
  "Ter" = "Terrace",
  "Hwy" = "Highway",
  "Way" = "Way",
  "Trl" = "Trail",
  "Plz" = "Plaza",
  "Ctr" = "Center",
  "Expwy" = "Expressway"
)
expand_abbreviations <- function(df, address_column) {
  # Iterate over each abbreviation and its long form
  for (abbr in names(abbreviations)) {
    long_form <- abbreviations[abbr]
    # Replace abbreviations with their long forms
    df[[address_column]] <- str_replace_all(df[[address_column]], 
                                            paste0("\\b", abbr, "\\b"), 
                                            long_form)
  }
  return(df)
}

#### 
