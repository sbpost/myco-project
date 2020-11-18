clean_data <- function(tbl) {
  
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