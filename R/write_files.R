write_files <- function(
  tbl_ls,
  corp_path,
  corp_filings_path,
  activities_path,
  addresses_path,
  mortgages_path,
  officers_path
) {
 
  # Corp 
  write_csv(
    tbl_ls$corp_tbl,
    file = corp_path
  )
  
  # CorpFilings
  write_csv(
    tbl_ls$corp_filings_tbl,
    file = corp_filings_path
  )
  
  # Activities
  write_csv(
    tbl_ls$activities_tbl,
    file = activities_path 
  )
  
   # Addresses
  write_csv(
    tbl_ls$addresses_tbl,
    file = addresses_path 
  )
  
  # Mortgages
  write_csv(
    tbl_ls$mortgages_tbl,
    file = mortgages_path
  )
  
  # Officers
  write_csv(
    tbl_ls$officers_tbl,
    file = officers_path
  )
}