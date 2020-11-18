curl_company_ids <- function() {
# This script collects company records from the
# https://www.myco.dica.gov.mm/ site. The script takes two parts. First
# it collects all the unique company ID's on the site. This is done 
# through the search API. Second, it uses the collected ID's to collect
# individual company records through the GetCorpProfile API. The content
# downloaded is saved as an .rds file.
# Data is cleaned and prepped in another script.

# Get company IDs -----------------------------------------------------
# A main issue with the MyCo site is that there is no documentation of
# an API. The interface used in the script seems to be for internal
# use. This also means that there is no documentation of any way to 
# retrieve all records at once. To get all the company IDs, it is then
# necessary to perform searches that return all the records at least 
# once. 
# We therefor run a search for each lower-case letter (a-z) and for 
# numbers 0-9. After, any non-unique row is removed. The search is not
# case sensitive. In the search, as it is currently, any re-registered
# company is included, but companies pending registration is not. This
# is the MyCo default.

# Create vector of search terms
search_terms <- c(letters, as.character(0:9))

# Page size is set to 99 999. Standard is to return 1000 records per 
# page. It would be best to return all records on one page, but a page
# size at 100 000 and up defaults to 1000 records. 
page_size <- 99999

# The page size limit means that some searches needs to add an additional
# search for the same term, but for page 2.

# Create urls
api_query_urls <-
  str_glue(
    "https://www.myco.dica.gov.mm/Api/Corp/SearchCorp?Term={search_term}&ExcludePending=true&ExcludeNotReReg=false&UseGenericSearch=false&PaginationPageSize={page_size}&PaginationCurrentPage={current_page}&OrderAscending=true&OrderBy=Name",
    search_term = search_terms,
    page_size = page_size,
    current_page = 1
  )

# Create loop that fetches all the records from the search API.

# Define a function that fetches the records from the search API. 
grab_company_records <- function(query_url) {
  response <- curl_fetch_memory(query_url)
  # Content is returned in raw form (digits). Reformat into json and 
  # then into a readable format
  content <-
    response$content %>%
    rawToChar() %>%
    fromJSON()

  # Check if all have been returned: is current records = expected 
  #records. If they haven't, go to next page and grab records. And so on.
while(content$records %>% nrow() != content$totalRecordCount) {

	#         Get current_page
	current_page <- 
		query_url %>%
		str_extract(pattern = "CurrentPage=[0-9]") %>%
		str_remove(pattern = "CurrentPage=") %>%
		as.numeric()

	#         Create next page url (basically, switch out CurrentPage=i to
	#                               CurrentPage=i+1).
	query_url <- 
		query_url %>%
		str_replace(
			    pattern = "CurrentPage=[0-9]",
			    replacement = str_glue("CurrentPage={current_page + 1}")
		)

		#                 Fetch next page and content
		new_response <- curl_fetch_memory(query_url)

		new_content <-
			new_response$content %>%
			rawToChar() %>%
			fromJSON()
     # 
     # # Append next page content to current content
		content$records <- bind_rows(content$records, new_content$records)

   } # while loop end
   
  # Attach request status to the content for quality control. 
  # If not 200 something went wrong.
  content$status <- response$status
  return(content)
  
} # function ends

# Apply function to the query urls: 
tictoc::tic()
fetched_records <- map(api_query_urls, grab_company_records)
tictoc::toc()
# Check status on request
http_status <- map_dbl(fetched_records, function(x) {x$status})
if(any(http_status != 200)) {
  stop("Not all GET requests for company records went through.")
}

# Extract records into one single data frame + remove duplicates
records_tbl <- 
  map(fetched_records, function(x) {x$records %>% as_tibble()}) %>%
  map_dfr(bind_rows) %>%
  distinct()

# Clean up env
rm(list = ls()[ls() != "records_tbl"])


company_ids <- records_tbl$CorpId %>% unique()
return(company_ids)
} 





# Get company data ----------------------------------------------------

get_company_profiles <- function(company_ids) {

corp_query_urls <-
  str_glue("https://www.myco.dica.gov.mm/Api/Corp/GetCorpProfile?id={company_ids}")

# Initiate pool of requests
url_pool <- new_pool()

# Collection list
raw_content_ls <- list()

# After a request is completed, grab raw content into collection list
grab_res <- function(res){
  raw_content_ls <<- c(raw_content_ls, list(res$content))
}

# Add each query to the request pool 
map(corp_query_urls,
    curl_fetch_multi,
    done = grab_res,
    pool = url_pool)

# Perform request async
# 50 000 companies takes around 70 minutes
request_status <- multi_run(pool = url_pool)

# Make sure that each request were successful
if(request_status$error > 0 | request_status$pending > 0) {
  stop("There were unsuccessful requests to company data API.")
}

# Reformat the company data and grab the relevant into data frames
content_ls <- 
  map(
    raw_content_ls,
    function(x) {x %>% rawToChar() %>% fromJSON()}
  )

return(content_ls)
}

