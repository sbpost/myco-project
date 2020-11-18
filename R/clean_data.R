clean_data <- function(tbl) {
  # This function performs some cursory cleaning of the variables
  # before they are written to file. Currently, only some obvious 
  # missing values ("" in string vars) are made missing (NA_character)
  # and potential duplicate records are removed. I still leave in 
  # entries called "N/A" by the source data. 
  
  # Turn "" into NA values for str variables
  fix_empty_str <- function(x) {
    x <- ifelse(x == "", NA_character_, x)
    return(x)
  }
  
  tbl <- 
    tbl %>%
    mutate(
      across(where(is.character), ~ fix_empty_str(.x))
    )
  
  # Remove all completely identical rows. Note: there shouldn't be any since 
  # CorpId is unique, but there could be mistakes at the server-side.
  tbl <-
    tbl %>%
    distinct()
  
  return(tbl)
}