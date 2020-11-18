the_plan <-
  drake::drake_plan(
    # Get all unique IDs through the search API
    company_ids = curl_company_ids(),
    # Grab records based on the IDs through the CorpProfile API.
    # While the following "batching" is very unelegant, something
    # seems to slow down if the process is not split up.
    company_records_1t10k = get_company_profiles(company_ids[1:10000]),
    company_records_10kt20k = get_company_profiles(company_ids[10001:20000]),
    company_records_20kt30k = get_company_profiles(company_ids[20001:30000]),
    company_records_30kt40k = get_company_profiles(company_ids[30001:40000]),
    company_records_40kt50k = get_company_profiles(company_ids[40001:50000]),
    company_records_50kt60k = get_company_profiles(company_ids[50001:60000]),
    company_records_60kt70k = get_company_profiles(company_ids[60001:70000]),
    company_records_70kt80k = get_company_profiles(company_ids[70001:80000]),
    company_records_80kt90k = get_company_profiles(company_ids[80001:90000]),
    company_records_90kt100k = get_company_profiles(company_ids[90001:100000]),
    company_records_100kt110k = get_company_profiles(company_ids[100001:110000]),
    company_records_110kplus = get_company_profiles(company_ids[-c(1:110000)]),
    # Gather in list
    company_records_list = list(
      company_records_1t10k,
      company_records_10kt20k,
      company_records_20kt30k,
      company_records_30kt40k,
      company_records_40kt50k,
      company_records_50kt60k,
      company_records_60kt70k,
      company_records_70kt80k,
      company_records_80kt90k,
      company_records_90kt100k,
      company_records_100kt110k,
      company_records_110kplus
    ),
    # Prepare data ----------------------------------
    # Extract records into data frames
    tables_ls = extract_records(curled_records = company_records_list),
    # Perform some cursory cleaning (fix NAs)
    clean_tbl_ls = map(tables_ls, clean_data),
    # Write data ------------------------------------
    out = write_files(
      tbl_ls = clean_tbl_ls,
      corp_path = file_out("data/corp.csv"),
      corp_filings_path = file_out("data/corp_filings.csv"),
      activities_path = file_out("data/activities.csv"),
      addresses_path = file_out("data/addresses.csv"),
      mortgages_path = file_out("data/mortgages.csv"),
      officers_path = file_out("data/officers.csv")
    )
  )