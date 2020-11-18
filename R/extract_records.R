extract_records <- function(curled_records) {
  # Method: 
  # All the scraped records is in a list, with each item having 10 000 records. I
  # first flatten the list to a single list of all the records (~ 120K records).
  # I then map the different extraction functions to the list to get the data. 
  # These are reshaped to a tibble. The extracted content is kept in a list
  # that contains:
  # corp_tbl: variables from `Corp`
  # corp_filings_tbl: variables from `CorpFilings`
  # activities_tbl: variables from `Activities`
  # addresses_tbl: variables from `Addresses`
  # mortgages_tbl: variables from `Mortgages`
  # officers_tbl: variables from `Officers`
  # 
  # The rest of the sections ("OfficersHistory", "Members", "MembersHistory",
  # "CommitteeMembers", "CommitteeMembersHistory", "Authorities", "Shares", 
  # "IsAuthorized", "IsAuthorizedToViewHistory", "IsLoggedin", "Correspondence",
  # "Documents", "Activities") are either related to technical stuff on the 
  # website, or is completely unpopulated (i.e. they don't contain any values 
  # for any companies).
   
  # Flatten scraping list to one long list
  content_ls <- unlist(curled_records, recursive = FALSE)
  rm(curled_records)
  
  ##----------------------------------------------------------------
  ##            1: Define functions that extracts data             -
  ##----------------------------------------------------------------
  
  # For each get_* function, I first need turn all NULL (non-populated)
  # entries into NA values, so I can remove them more easily later. I do
  # this ins the get_* functions to escape the function with an NA value and 
  # move to next entry.
  
  # For those entries that are populated, I also need to turn any NULL value
  # into NA, so I can create a tibble from the list objects. I do this
  # in the following function, that is reused in the get_* functions.
  
  prep_values <- function(record_item) {
      
    # Replace all NULL items with NA (column value will then just be "NA")
    record_item <- 
      record_item %>%
      replace(lengths(.)  == 0, NA) 
    
    return(record_item)
    
    } 
    
  
  # Corp ----------------------------------------------------------------
  
  get_corp <- function(record) {
    
    # Replace all NULL items with NA. These NA items are removed in get_df().
    if(length(record$Corp) == 0) {
      return(NA)
    }
   
    record$Corp <- prep_values(record_item = record$Corp) 
    
    # Replace all NULL items with NA. These NA items are removed in get_df().
    record$Corp <- 
      record$Corp %>%
      replace(lengths(.)  == 0, NA) 
    
    # Grab records
    corp_tbl <- as_tibble(record$Corp) %>%
      select(-c(ValidationResults, AnyErrors))
    
    return(corp_tbl)
  }
  # ---------------------------------------------------------------------
  
  # Corp filings --------------------------------------------------------
  
  get_corp_filings <- function(record) {
    
    # Replace all NULL items with NA. These NA items are removed in get_df().
    if(length(record$CorpFilings) == 0) {
      return(NA)
    }
    
    record$CorpFilings <- prep_values(record_item = record$CorpFilings) 
    
    # Save the company ID
    record$CorpFilings$CorpId <- record$Corp$CorpId
    
    corp_filing_tbl <- as_tibble(record$CorpFilings) %>%
      select(-c(ValidationResults, AnyErrors, Attachments, Responses))
    
    return(corp_filing_tbl)
    
  }
  # --------------------------------------------------------------------
  
  # Activities ----------------------------------------------------------
  get_activities <- function(record) {
    
    # Replace all NULL items with NA. These NA items are removed in get_df().
    if(length(record$Activities) == 0) {
      return(NA)
    }
    
    record$Activities <- prep_values(record_item = record$Activities) 
    
    # Add corp ID
    record$Activities$CorpId <- record$Corp$CorpId
    
    # Grab records
    activities_tbl <- as_tibble(record$Activities) %>%
      select(-c(ValidationResults, AnyErrors))
    
    return(activities_tbl)
  }
  # --------------------------------------------------------------------
  
  # Addresses -----------------------------------------------------------
  get_addresses <- function(record) {
    
    # Replace all NULL items with NA. These NA items are removed in get_df().
    if(length(record$Addresses) == 0) {
      return(NA)
    }
    
    record$Addresses <- prep_values(record_item = record$Addresses) 
    
    # Add corp ID
    record$Addresses$CorpId <- record$Corp$CorpId
    
    # The number of rows in Addresses and Adresses$Adress is always the same,
    # that is, it maps 1-to-1. I therefor just add the information in 
    # Addresses$Address to the Addresses data frame. This increases the number
    # of columns, but not the number of rows.
    
    addresses_tbl <- 
      record$Addresses %>%
      tibble() %>%
      select(-c(Address, ValidationResults, AnyErrors))
    
    address_tbl <- 
      record$Addresses$Address %>%
      tibble() %>%
      # deselect duplicated cols
      select(-c(UIFormattedAddress, ValidationResults, AnyErrors))
    
    addresses_tbl <- bind_cols(addresses_tbl, address_tbl)
    
    return(addresses_tbl)
  }
  # --------------------------------------------------------------------
  
  # Mortages -----------------------------------------------------------
  get_mortgages <- function(record) {
    
    # Replace all NULL items with NA. These NA items are removed in get_df().
    if(length(record$Mortgages) == 0) {
      return(NA)
    }
    
    record$Mortgages <- prep_values(record_item = record$Mortgages) 
    
    # grab corp id
    record$Mortgages$CorpId <- record$Corp$CorpId
    
    # `Receiver` is nested within `Mortgages`. Inside `Receiver` is nested `Address` and
    # `SecondaryAddress`. It seems like `Receiver` is mostly empty. `Address` and 
    # `SecondaryAddress` is tied to a an Officer ID, which is also mostly empty. 
    # Without tying it to a specific officer, the information does not have value.
    # I don't include the address-parts. 
    
    # Grab records
    mortgages_tbl <- as_tibble(record$Mortgages) %>%
      select(-c(Receiver, ValidationResults, AnyErrors))
    
    receiver_tbl <- 
      record$Mortgages$Receiver %>%
      # Remove duplicates (also present in Mortgages)
      # Also remove the Address and SecondaryAddress
      select(-c(ValidationResults, AnyErrors, CorpId, 
                Address, SecondaryAddress, Shares)) %>%
      tibble() %>%
      # Rename vars so they are clearly distinguished from the mortages info
      rename_with(~ str_glue("Receiver{.x}"))
    
    mortgages_tbl <- 
      bind_cols(mortgages_tbl, receiver_tbl)
    
    # Return
    return(mortgages_tbl)
  }
  
  # --------------------------------------------------------------------
  
  # Officers -----------------------------------------------------------
  get_officers <- function(record) {
    
    # Replace all NULL items with NA. These NA items are removed in get_df().
    if(length(record$Officers) == 0) {
      return(NA)
    }
    
    record$Officers <- prep_values(record_item = record$Officers) 
    
    # Officer information
    officers_tbl <- 
      record$Officers %>%
      tibble() %>%
      select(-c(Address, SecondaryAddress, Shares, ValidationResults, AnyErrors))
    
    # Officers$Address and Officers are always the same length. 
    # They map 1-to-1. I just add the columns in the Address to the
    # officer column.
    officers_address_tbl <-
      record$Officers$Address %>% 
      tibble() %>%
      select(-c(Address, UIFormattedAddress, ValidationResults, AnyErrors))
    
    # Is same length as officers address, just more info.
    officers_address_address_tbl <-
      record$Officers$Address$Address %>%
      tibble() %>%
      select(-c(UIFormattedAddress, ValidationResults, AnyErrors))
    
    # Add a prefix on address variables to distinguish them from officers
    # variables (effectiveDateFrom, effectiveDateTo, etc).
    officers_address_tbl <- 
      bind_cols(officers_address_tbl, officers_address_address_tbl) %>%
      rename_with(~str_glue("Address{.x}"))
    
    final_officers_tbl <-
      bind_cols(officers_tbl, officers_address_tbl)
    
    return(final_officers_tbl)
  }
  
  ##---------------------------------------------------------------
  ##        2: Apply functions and extract data into tbls         -
  ##---------------------------------------------------------------
  # Remove observations that are missing and flatten the lists into
  # a data frame.
  get_df <- function(record_ls) {
    
    # Get index of which items in the list are NA. Length = 
    # number of cols. No items have 1 col except NAs.
    l_index <- map_lgl(record_ls, ~ length(.x) == 1)
    
    # Remove the indexed records and flatten list to
    # tibble 
    records_tbl <-
      record_ls[!l_index] %>% 
      map_dfr(bind_rows)
    
    return(records_tbl)
    
  }
  
  # Extract content
  extracted_ls <- list()
  
  extracted_ls$corp_tbl <- 
    map(content_ls, get_corp) %>%
    get_df()
  
  extracted_ls$corp_filings_tbl <-
    map(content_ls, get_corp_filings) %>%
    get_df()
  
  extracted_ls$addresses_tbl <-
    map(content_ls, get_addresses) %>%
    get_df() 
  
  extracted_ls$activities_tbl <-
    map(content_ls, get_activities) %>%
    get_df()
  
  extracted_ls$mortgages_tbl <-
    map(content_ls, get_mortgages) %>%
    get_df()
  
  extracted_ls$officers_tbl <-
    map(content_ls, get_officers) %>%
    get_df()
  
  return(extracted_ls)
}
  
