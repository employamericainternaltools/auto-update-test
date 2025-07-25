# Load necessary libraries
rm(list = ls())
library(readxl)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(httr)
library(stringr)
library(lubridate)
library(jsonlite)


naics_dict <- read_csv("naics dictionary.csv")
naics_lookup <- setNames(naics_dict$name, naics_dict$code)


#Importing Main Data

# CES Ingestion
{
    data_type_lookup <- c(
      "01" = "All Employees",
      "02" = "Average Weekly Hours of All Employees",
      "03" = "Average Hourly Earnings of All Employees",
      "04" = "Average Weekly Overtime Hours of All Employees",
      "06" = "Production and Nonsupervisory Employees",
      "07" = "Average Weekly Hours of Production and Nonsupervisory Employees",
      "08" = "Average Hourly Earnings of Production and Nonsupervisory Employees",
      "09" = "Average Weekly Overtime Hours of Production and Nonsupervisory Employees",
      "10" = "Women Employees",
      "11" = "Average Weekly Earnings of All Employees",
      "12" = "Average Weekly Earnings of All Employees",
      "13" = "Average Hourly Earnings of All Employees",
      "15" = "Average Hourly Earnings of All Employees, Excluding Overtime",
      "16" = "Indexes of Aggregate Weekly Hours of All Employees",
      "17" = "Indexes of Aggregate Weekly Payrolls of All Employees",
      "19" = "Average Weekly Hours of All Employees, Quarterly Averages, Seasonally Adjusted",
      "20" = "Average Weekly Overtime Hours of All Employees, Quarterly Averages, Seasonally Adjusted",
      "21" = "Diffusion Indexes, 1-Month Span, Seasonally Adjusted",
      "22" = "Diffusion Indexes, 3-Month Span, Seasonally Adjusted",
      "23" = "Diffusion Indexes, 6-Month Span, Seasonally Adjusted",
      "24" = "Diffusion Indexes, 12-Month Span, Not Seasonally Adjusted",
      "25" = "All Employees, Quarterly Averages, Seasonally Adjusted",
      "26" = "All Employees, 3-Month Average Change, Seasonally Adjusted",
      "30" = "Average Weekly Earnings of Production and Nonsupervisory Employees",
      "31" = "Average Weekly Earnings of Production and Nonsupervisory Employees",
      "32" = "Average Hourly Earnings of Production and Nonsupervisory Employees",
      "33" = "Average Hourly Earnings of Production and Nonsupervisory Employees, Excluding Overtime",
      "34" = "Indexes of Aggregate Weekly Hours of Production and Nonsupervisory Employees",
      "35" = "Indexes of Aggregate Weekly Payrolls of Production and Nonsupervisory Employees",
      "36" = "Average Weekly Hours, Production/Nonsupervisory Employees, Quarterly Averages, Seasonally Adjusted",
      "37" = "Average Weekly Overtime Hours, Production/Nonsupervisory Employees, Quarterly Averages, Seasonally Adjusted",
      "38" = "Production and Nonsupervisory Employees-to-All Employees Ratio",
      "39" = "Women Employees-to-All Employees Ratio",
      "56" = "Aggregate Weekly Hours of All Employees",
      "57" = "Aggregate Weekly Payrolls of All Employees",
      "58" = "Aggregate Weekly Overtime Hours of All Employees",
      "81" = "Aggregate Weekly Hours of Production and Nonsupervisory Employees",
      "82" = "Aggregate Weekly Payrolls of Production and Nonsupervisory Employees",
      "83" = "Aggregate Weekly Overtime Hours of Production and Nonsupervisory Employees, Thousands"
    )
    
    # Create units lookup based on data_type_code
    units_lookup <- c(
      "01" = "Thousands of Employees",
      "02" = "Hours",
      "03" = "Dollars",
      "04" = "Hours",
      "06" = "Thousands of Employees",
      "07" = "Hours",
      "08" = "Dollars",
      "09" = "Hours",
      "10" = "Thousands of Employees",
      "11" = "Dollars",
      "12" = "1982-1984 Dollars",
      "13" = "1982-1984 Dollars",
      "15" = "Dollars",
      "16" = "Index, 2007 = 100",
      "17" = "Index, 2007 = 100",
      "19" = "Hours",
      "20" = "Hours",
      "21" = "Index, 50 = Matched",
      "22" = "Index, 50 = Matched",
      "23" = "Index, 50 = Matched",
      "24" = "Index, 50 = Matched",
      "25" = "Thousands of Employees",
      "26" = "Thousands of Employees",
      "30" = "Dollars",
      "31" = "1982-1984 Dollars",
      "32" = "1982-1984 Dollars",
      "33" = "Dollars",
      "34" = "Index, 2002 = 100",
      "35" = "Index, 2002 = 100",
      "36" = "Hours",
      "37" = "Hours",
      "38" = "Ratio",
      "39" = "Ratio",
      "56" = "Hours",
      "57" = "Dollars",
      "58" = "Hours",
      "81" = "Hours",
      "82" = "Dollars",
      "83" = "Hours"
    )
    
    # URL of the data
    url <- "https://download.bls.gov/pub/time.series/ce/ce.data.0.AllCESSeries"
    
    # Set user agent with a sample email address
    user_agent <- "user@example.com"
    
    # Download the data using GET with the user agent
    response <- GET(url, user_agent(user_agent))
    
    # Read the content into a dataframe
    bls_data <- read_delim(content(response, "text"), 
                           delim = "\t", 
                           col_names = TRUE, 
                           trim_ws = TRUE)
    
    # Parse the series_id column, convert seasonal_adjustment to logical,
    # process supersector_industry_codes, and convert data_type_code to text
    bls_data <- bls_data %>%
      mutate(
        prefix = str_sub(series_id, 1, 2),
        seasonal_adjustment = str_sub(series_id, 3, 3) == "S",
        supersector_industry_codes = str_sub(series_id, 4, 11),
        data_type_code = str_sub(series_id, 12, 13),
        # Process supersector_industry_codes
        supersector_industry_codes = substr(supersector_industry_codes, 3, 8),  # Remove first two digits
        supersector_industry_codes = str_replace(supersector_industry_codes, "0.*$", ""),  # Truncate after first zero
        # Convert data_type_code to text
        data_type_text = data_type_lookup[data_type_code],
        # Add Units based on data_type_code
        Units = units_lookup[data_type_code]
      )
    
    # Create Date column and process the dataframe
    bls_data <- bls_data %>%
      mutate(
        month = as.integer(str_extract(period, "\\d+")),
        Date = ymd(paste(year, month, "01", sep = "-"))
      ) %>%
      select(-month, -year, -period, -footnote_codes, -data_type_code, -series_id, -prefix) %>% 
      filter(!is.na(Date)) %>%
      rename(
        NAICS_Code = supersector_industry_codes,
        Value = value,
        Indicator = data_type_text
      ) %>%
      mutate(
        Dataset = "Current Employment Statistics",
        NAICS_Name = naics_lookup[NAICS_Code]  # Add NAICS name mapping
      ) %>%
      select(
        Date, Dataset, Indicator, NAICS_Code, NAICS_Name, seasonal_adjustment, Units, Value
      ) %>%
      filter(trimws(NAICS_Code) != "")
    
    bls_data <- bls_data %>%
      mutate(
        # Check if Units is "Thousands of Employees"
        Value = ifelse(Units == "Thousands of Employees", Value * 1000, Value),
        # Update Units label for those records
        Units = ifelse(Units == "Thousands of Employees", "Employees", Units)
      )
    
    # Save the dataframe as an RDS file (R's native format for storing single objects)
    CES_tidy <- bls_data
  }

# Exports Ingestion
{
  
  # Define the API endpoint URL
  api_url <- "https://api.census.gov/data/timeseries/intltrade/exports/naics"
  
  # Define query parameters
  params <- list(
    get = "MONTH,YEAR,NAICS,ALL_VAL_MO"
  )
  
  # Make the API request
  response <- GET(url = api_url, query = params)
  
  # Parse the JSON content
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  # Convert to a dataframe
  df <- as.data.frame(data[-1,])  # Remove the first row (column names)
  colnames(df) <- data[1,]  # Set column names from the first row
  rm(data, response)
  
  # Convert columns to appropriate data types
  df$MONTH <- as.integer(df$MONTH)
  df$YEAR <- as.integer(df$YEAR)
  df$NAICS <- as.character(df$NAICS)
  df$ALL_VAL_MO <- as.numeric(df$ALL_VAL_MO)
  
  # Create a new Date column
  df$Date <- ymd(paste(df$YEAR, df$MONTH, "01", sep = "-"))
  
  # Reorder columns and process NAICS codes
  df <- df %>%
    select(Date, NAICS, ALL_VAL_MO) %>%
    rename(
      NAICS_Code = NAICS,
      Value = ALL_VAL_MO
    ) %>%
    # Remove rows with NAICS_Code "31-33" or containing "X"
    filter(NAICS_Code != "31-33" & !grepl("X", NAICS_Code)) %>%
    # Replace "-" with "0" in NAICS_Code
    mutate(
      NAICS_Code = ifelse(NAICS_Code == "-", "0", NAICS_Code),
      Dataset = "International Trade",
      Units = "Dollars",
      Indicator = "Exports",
      NAICS_Name = naics_lookup[NAICS_Code],
      seasonal_adjustment = FALSE
    ) %>%
    select(Date, Dataset, Indicator, NAICS_Name, NAICS_Code, seasonal_adjustment, Units, Value)
  
  Exports_tidy <- df
  
}

# Imports Ingestion
{
  # Define the API endpoint URL
  api_url <- "https://api.census.gov/data/timeseries/intltrade/imports/naics"
  
  # Define query parameters
  params <- list(
    get = "MONTH,YEAR,NAICS,GEN_VAL_MO"
  )
  
  # Make the API request
  response <- GET(url = api_url, query = params)
  
  # Parse the JSON content
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  # Convert to a dataframe
  df <- as.data.frame(data[-1,])  # Remove the first row (column names)
  colnames(df) <- data[1,]  # Set column names from the first row
  rm(data, response)
  
  # Convert columns to appropriate data types
  df$MONTH <- as.integer(df$MONTH)
  df$YEAR <- as.integer(df$YEAR)
  df$NAICS <- as.character(df$NAICS)
  df$GEN_VAL_MO <- as.numeric(df$GEN_VAL_MO)
  
  # Create a new Date column
  df$Date <- ymd(paste(df$YEAR, df$MONTH, "01", sep = "-"))
  
  # Reorder columns and process NAICS codes
  df <- df %>%
    select(Date, NAICS, GEN_VAL_MO) %>%
    rename(
      NAICS_Code = NAICS,
      Value = GEN_VAL_MO
    ) %>%
    # Remove rows with NAICS_Code "31-33" or containing "X"
    filter(NAICS_Code != "31-33" & !grepl("X", NAICS_Code)) %>%
    # Replace "-" with "0" in NAICS_Code
    mutate(
      NAICS_Code = ifelse(NAICS_Code == "-", "0", NAICS_Code),
      Dataset = "International Trade",
      Indicator = "Imports",
      Units = "Dollars",
      NAICS_Name = naics_lookup[NAICS_Code],
      seasonal_adjustment = FALSE
    ) %>%
    select(Date, Dataset, Indicator, NAICS_Name, NAICS_Code, Units, seasonal_adjustment, Value)
  
  Imports_tidy <- df
  
}

# Create Trade Balance dataset
{
  # Join Exports and Imports datasets on Date and NAICS_Code
  Balance_tidy <- Exports_tidy %>%
    inner_join(
      Imports_tidy,
      by = c("Date", "NAICS_Code", "NAICS_Name", "Dataset", "seasonal_adjustment"),
      suffix = c("_exports", "_imports")
    ) %>%
    # Calculate the trade balance (Exports - Imports)
    mutate(
      Indicator = "Trade Balance",
      Units = "Dollars",
      Value = Value_exports - Value_imports
    ) %>%
    # Select and reorder columns to match the standard structure
    select(Date, Dataset, Indicator, NAICS_Name, NAICS_Code, Units, seasonal_adjustment, Value)
}

# G17 Ingestion
{# Step 1: Read the data into a single-column dataframe
  url <- "https://www.federalreserve.gov/releases/g17/ipdisk/alltables.txt"
  df_raw <- read_lines(url) %>%
    as.data.frame() %>%
    setNames("raw_text")
  
  # Step 2: Define a function to separate quoted values
  separate_quoted <- function(text) {
    quoted <- str_extract(text, '^"[^"]*"')
    remainder <- str_replace(text, '^"[^"]*"\\s*', '')
    list(quoted = quoted, remainder = remainder)
  }
  
  # Step 3: Apply the function to create two new columns
  df_separated <- df_raw %>%
    mutate(separated = map(raw_text, separate_quoted)) %>%
    unnest_wider(separated)
  
  # Step 4: Create a new "Year" column from the first four characters of "remainder"
  df_separated <- df_separated %>%
    mutate(Year = str_sub(remainder, 1, 4))
  
  # Step 5: Remove the first eleven characters from the "remainder" column
  df_separated <- df_separated %>%
    mutate(remainder = str_sub(remainder, 12))
  
  # Step 6: Manually split the "remainder" column into 12 columns, each 10 characters wide
  df_separated <- df_separated %>%
    mutate(
      col1 = str_sub(remainder, 1, 10),
      col2 = str_sub(remainder, 11, 20),
      col3 = str_sub(remainder, 21, 30),
      col4 = str_sub(remainder, 31, 40),
      col5 = str_sub(remainder, 41, 50),
      col6 = str_sub(remainder, 51, 60),
      col7 = str_sub(remainder, 61, 70),
      col8 = str_sub(remainder, 71, 80),
      col9 = str_sub(remainder, 81, 90),
      col10 = str_sub(remainder, 91, 100),
      col11 = str_sub(remainder, 101, 110),
      col12 = str_sub(remainder, 111, 120)
    ) %>%
    select(-remainder)  # Remove the original remainder column
  
  # Step 7: Trim whitespace from all columns
  df_cleaned <- df_separated %>%
    mutate(across(everything(), str_trim))
  
  # Step 8: Convert Year to integer and col1 to col12 to numeric
  df_cleaned <- df_cleaned %>%
    mutate(
      Year = as.integer(ifelse(Year == "", NA, Year)),
      across(starts_with("col"), ~as.numeric(ifelse(. == "", NA, .)))
    )
  
  # Step 9: Rename col1 to col12 with month names
  df_renamed <- df_cleaned %>%
    rename(
      January = col1,
      February = col2,
      March = col3,
      April = col4,
      May = col5,
      June = col6,
      July = col7,
      August = col8,
      September = col9,
      October = col10,
      November = col11,
      December = col12
    )
  
  # Step 10: Pivot the table to longer format
  df_long <- df_renamed %>%
    pivot_longer(
      cols = January:December,
      names_to = "Month",
      values_to = "Value"
    ) %>%
    select(-raw_text) %>%
    rename(series_code = quoted)
  
  # Step 10.5: pull out series code descriptions
  series_titles <- df_long %>%
    filter(is.na(Year)) %>%
    filter(Month == "January")
  
  # Step 11: Remove description rows
  df_long <- df_long %>%
    filter(!is.na(Year))
  
  # Step 12: Combine Month and Year into a Date column
  df_long <- df_long %>%
    mutate(
      Date = ymd(paste(Year, Month, "01")),
      Month = NULL,  # Remove the original Month column
      Year = NULL    # Remove the original Year column
    )
  
  # Step 13: Split series_code into Indicator and series_id, and remove quotation marks
  df_long <- df_long %>%
    mutate(
      Indicator = str_remove_all(str_extract(series_code, "^[^.]+"), '"'),
      series_id = str_remove_all(str_extract(series_code, "[^.]+$"), '"'),
      series_code = NULL  # Remove the original series_code column
    ) %>%
    select(Date, Indicator, series_id, Value)
  
  # Step 14: Add seasonal_adjustment column
  df_long <- df_long %>%
    mutate(
      seasonal_adjustment = str_sub(Indicator, -1) == "S"
    )
  
  # Step 15: Map all Indicator values including previously filtered ones
  df_long <- df_long %>%
    mutate(Indicator = case_when(
      str_starts(Indicator, "IPS") ~ "Industrial Production",
      str_starts(Indicator, "IPN") ~ "Industrial Production",
      str_starts(Indicator, "UTL") ~ "Capacity Utilization",
      str_starts(Indicator, "CAP") ~ "Capacity",
      str_starts(Indicator, "GVP") ~ "Gross Value of Products",
      str_starts(Indicator, "DIFFUSION_1") ~ "1-Month Diffusion Index",
      str_starts(Indicator, "DIFFUSION_3") ~ "3-Month Diffusion Index",
      str_starts(Indicator, "DIFFUSION_6") ~ "6-Month Diffusion Index",
      str_starts(Indicator, "RIWIP") ~ "Relative Importance Weights",
      TRUE ~ Indicator  # Keep original value if it doesn't match any of the above
    ))
  
  # Step 16: Create crosswalk with series_id and NAICS codes, keeping only the first occurrence of each NAICS code
  crosswalk_df <- series_titles %>%
    mutate(
      series_id = str_trim(str_remove(str_extract(series_code, "^[^:]+"), '^"')),  # Extract series_id and clean it
      NAICS_Code = str_extract(series_code, "(?<=NAICS=)\\d+")
    ) %>%
    filter(!is.na(NAICS_Code)) %>%  # Remove rows where NAICS code couldn't be extracted
    distinct(NAICS_Code, .keep_all = TRUE) %>% # Keep only the first occurrence of each NAICS code
    select(series_id, NAICS_Code)  
  
  #add the Aggregate
  crosswalk_df <- crosswalk_df %>%
    add_row(series_id = "B50001", NAICS_Code = "0")
  
  # Step 17: Join NAICS codes to main dataframe
  df_long <- df_long %>%
    left_join(crosswalk_df, by = "series_id") %>%
    # Remove the filter to keep all indicators
    # filter(!is.na(NAICS_Code)) %>%
    mutate(
      NAICS_Name = naics_lookup[NAICS_Code],
      Dataset = "Production and Capacity"
    )
  
  # Step 18: Add Units column based on Indicator mapping
  df_long <- df_long %>%
    mutate(Units = case_when(
      Indicator == "Industrial Production" ~ "Index, 2017 = 100",
      Indicator == "Capacity Utilization" ~ "Percent",
      Indicator == "Capacity" ~ "Index, 2017 = 100",
      Indicator == "Gross Value of Products" ~ "Dollars",
      Indicator == "1-Month Diffusion Index" ~ "Index",
      Indicator == "3-Month Diffusion Index" ~ "Index",
      Indicator == "6-Month Diffusion Index" ~ "Index",
      Indicator == "Relative Importance Weights" ~ "Percent",
      TRUE ~ NA_character_  # Default to NA if no match
    ))
  
  # Divide values by 100 for rows with Units = "Percent" 
  df_long <- df_long %>%
    mutate(
      Value = ifelse(Units == "Percent", Value / 100, Value)
    )
  
  # Final selection and ordering of columns
  G17_tidy <- df_long %>%
    select(Date, Dataset, Indicator, Units, NAICS_Name, NAICS_Code, seasonal_adjustment, Value)}

# Investment Ingestion
{
  
  # URL of the file
  url <- "https://apps.bea.gov/national/Release/TXT/NipaDataQ.txt"
  
  df <- read_csv(url,
                 col_types = cols(.default = col_character()),  # Read everything as character first
                 quote = "\"",
                 trim_ws = TRUE)
  
  df <- df %>%
    # Rename %SeriesCode to X.SeriesCode
    rename(X.SeriesCode = `%SeriesCode`) %>%
    mutate(
      # Remove commas from Value column
      Value = gsub(",", "", Value),
      # Convert to numeric
      Value = as.numeric(Value)
    )
  
  # Read the file directly from the URL into a dataframe
#  df <- read.csv(url, header = TRUE, quote = "\"", sep = ",", 
#                 stringsAsFactors = FALSE)
  
  # Parse the "Period" column
  df$Date <- parse_date_time(df$Period, "yq")
  
  # Read the investment dictionary
  dictionary <- read_excel("investment dictionary.xlsx")
  
  # Join df with dictionary and keep only matching rows
  df_filtered <- df %>%
    inner_join(dictionary, by = "X.SeriesCode")
  
  df_filtered <- df_filtered %>%
    mutate(
      Dataset = "Investment",
      seasonal_adjustment = FALSE
    ) %>%
    select(Date, Dataset, Indicator, NAICS_Name, NAICS_Code, Units, seasonal_adjustment, Value) %>%
#    mutate(Value = parse_number(Value)) %>%
    mutate(Value = Value *1000000)
  
  Investment_tidy <- df_filtered %>%
    group_by(Indicator) %>%
    arrange(NAICS_Code) %>%
    ungroup()
  
}

# Import Price Index Ingestion
{
  
  # URL of the data
  url <- "https://download.bls.gov/pub/time.series/ei/ei.data.03.NAICSImport"
  
  # Set user agent with a sample email address
  user_agent <- "user@example.com"
  
  # Download the data using GET with the user agent
  response <- GET(url, user_agent(user_agent))
  
  # Read the content into a dataframe
  bls_data <- read_delim(content(response, "text"), 
                         delim = "\t", 
                         col_names = TRUE, 
                         trim_ws = TRUE)
  
  #Parse the series_id column, (source: https://www.bls.gov/help/hlpforma.htm#PC)
  
  #convert seasonal_adjustment to logical,
  
  #process supersector_industry_codes to NAICS codes
  
  #convert data_type_code to text
  bls_data <- bls_data %>%
    mutate(
      prefix = str_sub(series_id, 1, 2),
      seasonal_adjustment = str_sub(series_id, 3, 3) == "S",
      supersector_industry_codes = as.character(str_sub(series_id, 6, 11)),
      # Process supersector_industry_codes
      supersector_industry_codes = str_replace(supersector_industry_codes, "0.*$", "")  # Truncate after first zero
    ) %>%
    filter(!is.na(supersector_industry_codes))
  
  # Create Date column 
  
  #process the dataframe to Date | Dataset | Indicator | NAICS_Name | NAICS_Code | seasonal_adjustment | Value format
  bls_data <- bls_data %>%
    mutate(
      month = as.integer(str_extract(period, "\\d+")),
      Date = ymd(paste(year, month, "01", sep = "-"))
    ) %>%
    select(-month, -year, -period, -footnote_codes, -series_id, -prefix) %>% 
    filter(!is.na(Date)) %>%
    rename(
      NAICS_Code = supersector_industry_codes,
      Value = value
    ) %>%
    mutate(
      Dataset = "Price Indices",
      Indicator = "Import Price Index",
      NAICS_Name = naics_lookup[NAICS_Code]  # Add NAICS name mapping
    ) %>%
    select(
      Date, Dataset, Indicator, NAICS_Code, NAICS_Name, seasonal_adjustment, Value
    ) %>%
    filter(trimws(NAICS_Code) != "")
  
  # Reindex values to December 2024 using summarize and join approach
  # First, get the December 2024 values for each NAICS_Code and seasonal_adjustment
  dec_2024_values <- bls_data %>%
    filter(Date == ymd("2024-12-01")) %>%
    # In case there are multiple December 2024 values for the same group, take the first one
    group_by(NAICS_Code, seasonal_adjustment) %>%
    summarize(dec_2024_value = first(Value), .groups = "drop")
  
  # Join with the original data and calculate the reindexed values
  bls_data_reindexed <- bls_data %>%
    left_join(dec_2024_values, by = c("NAICS_Code", "seasonal_adjustment")) %>%
    # Reindex to make December 2024 = 100
    mutate(
      Value = (Value / dec_2024_value) * 100,
      Units = "Index, Dec 2024 = 100"
    ) %>%
    select(-dec_2024_value)  # Remove the temporary column
  
  # Final selection with the new Units column
  IPI_tidy <- bls_data_reindexed %>%
    select(
      Date, Dataset, Indicator, NAICS_Name, NAICS_Code, Units, seasonal_adjustment, Value
    )
  
}

# Export Price Index Ingestion
{
  
  # URL of the data
  url <- "https://download.bls.gov/pub/time.series/ei/ei.data.04.NAICSExport"
  
  # Set user agent with a sample email address
  user_agent <- "user@example.com"
  
  # Import NAICS dictionary
  naics_dict <- read_csv("naics dictionary.csv")
  naics_lookup <- setNames(naics_dict$name, naics_dict$code)
  
  # Download the data using GET with the user agent
  response <- GET(url, user_agent(user_agent))
  
  # Read the content into a dataframe
  bls_data <- read_delim(content(response, "text"), 
                         delim = "\t", 
                         col_names = TRUE, 
                         trim_ws = TRUE)
  
  #Parse the series_id column, (source: https://www.bls.gov/help/hlpforma.htm#PC)
  
  #convert seasonal_adjustment to logical,
  
  #process supersector_industry_codes to NAICS codes
  
  #convert data_type_code to text
  bls_data <- bls_data %>%
    mutate(
      prefix = str_sub(series_id, 1, 2),
      seasonal_adjustment = str_sub(series_id, 3, 3) == "S",
      supersector_industry_codes = as.character(str_sub(series_id, 6, 11)),
      # Process supersector_industry_codes
      supersector_industry_codes = str_replace(supersector_industry_codes, "0.*$", "")  # Truncate after first zero
    ) %>%
    filter(!is.na(supersector_industry_codes))
  
  # Create Date column 
  
  #process the dataframe to Date | Dataset | Indicator | NAICS_Name | NAICS_Code | seasonal_adjustment | Value format
  bls_data <- bls_data %>%
    mutate(
      month = as.integer(str_extract(period, "\\d+")),
      Date = ymd(paste(year, month, "01", sep = "-"))
    ) %>%
    select(-month, -year, -period, -footnote_codes, -series_id, -prefix) %>% 
    filter(!is.na(Date)) %>%
    rename(
      NAICS_Code = supersector_industry_codes,
      Value = value
    ) %>%
    mutate(
      Dataset = "Price Indices",
      Indicator = "Export Price Index",
      NAICS_Name = naics_lookup[NAICS_Code]  # Add NAICS name mapping
    ) %>%
    select(
      Date, Dataset, Indicator, NAICS_Code, NAICS_Name, seasonal_adjustment, Value
    ) %>%
    filter(trimws(NAICS_Code) != "")
  
  # Reindex values to December 2024 using summarize and join approach
  # First, get the December 2024 values for each NAICS_Code and seasonal_adjustment
  dec_2024_values <- bls_data %>%
    filter(Date == ymd("2024-12-01")) %>%
    # In case there are multiple December 2024 values for the same group, take the first one
    group_by(NAICS_Code, seasonal_adjustment) %>%
    summarize(dec_2024_value = first(Value), .groups = "drop")
  
  # Join with the original data and calculate the reindexed values
  bls_data_reindexed <- bls_data %>%
    left_join(dec_2024_values, by = c("NAICS_Code", "seasonal_adjustment")) %>%
    # Reindex to make December 2024 = 100
    mutate(
      Value = (Value / dec_2024_value) * 100,
      Units = "Index, Dec 2024 = 100"
    ) %>%
    select(-dec_2024_value)  # Remove the temporary column
  
  # Final selection with the new Units column
  EPI_tidy <- bls_data_reindexed %>%
    select(
      Date, Dataset, Indicator, NAICS_Name, NAICS_Code, Units, seasonal_adjustment, Value
    )
  
}

# Producer Price Index Ingestion
{
  # URL of the data
  url <- "https://download.bls.gov/pub/time.series/pc/pc.data.0.Current"
  # Set user agent with a sample email address
  user_agent <- "user@example.com"
  # Create a lookup table for data type codes (source: https://download.bls.gov/pub/time.series/ce/ce.datatype)
  data_type_lookup <- c()
  # Import NAICS dictionary
  naics_dict <- read_csv("naics dictionary.csv")
  naics_lookup <- setNames(naics_dict$name, naics_dict$code)
  
  # Download the data using GET with the user agent
  response <- GET(url, user_agent(user_agent))
  
  # Read the content into a dataframe
  bls_data <- read_delim(content(response, "text"), 
                         delim = "\t", 
                         col_names = TRUE, 
                         trim_ws = TRUE)
  
  # Filter out rows where series_id is longer than 15 characters
  bls_data <- bls_data %>%
    filter(nchar(series_id) <= 15)
  
  #Parse the series_id column, (source: https://www.bls.gov/help/hlpforma.htm#PC)
  #convert seasonal_adjustment to logical,
  #process supersector_industry_codes to NAICS codes
  bls_data <- bls_data %>%
    mutate(
      prefix = str_sub(series_id, 1, 2),
      seasonal_adjustment = str_sub(series_id, 3, 3) == "S",
      supersector_industry_codes = str_sub(series_id, 4, 9)  # First just extract the string
    ) %>%
    mutate(
      # Remove hyphens from supersector values
      supersector_industry_codes = str_remove_all(supersector_industry_codes, "-")
    ) %>%
    filter(str_detect(supersector_industry_codes, "^[0-9]+$")) %>%  # Filter for numeric-only strings
    mutate(
      supersector_industry_codes = as.character(supersector_industry_codes),
    ) %>%
    filter(!is.na(supersector_industry_codes))
  
  # Create Date column 
  #process the dataframe to Date | Dataset | Indicator | NAICS_Name | NAICS_Code | seasonal_adjustment | Value format
  bls_data <- bls_data %>%
    mutate(
      month = as.integer(str_extract(period, "\\d+")),
      Date = ymd(paste(year, month, "01", sep = "-"))
    ) %>%
    select(-month, -year, -period, -footnote_codes, -series_id, -prefix) %>% 
    filter(!is.na(Date)) %>%
    rename(
      NAICS_Code = supersector_industry_codes,
      Value = value
    ) %>%
    mutate(
      Dataset = "Price Indices",
      Indicator = "Producer Price Index",
      NAICS_Name = naics_lookup[NAICS_Code]  # Add NAICS name mapping
    ) %>%
    mutate(
      NAICS_Code = as.character(NAICS_Code)  # Fixed assignment operator
    ) %>%
    select(
      Date, Dataset, Indicator, NAICS_Code, NAICS_Name, seasonal_adjustment, Value
    ) %>%
    filter(trimws(NAICS_Code) != "")
  
  # Reindex values to December 2024 using summarize and join approach
  # First, get the December 2024 values for each NAICS_Code and seasonal_adjustment
  dec_2024_values <- bls_data %>%
    filter(Date == ymd("2024-12-01")) %>%
    # In case there are multiple December 2024 values for the same group, take the first one
    group_by(NAICS_Code, seasonal_adjustment) %>%
    summarize(dec_2024_value = first(Value), .groups = "drop")
  
  # Join with the original data and calculate the reindexed values
  bls_data_reindexed <- bls_data %>%
    left_join(dec_2024_values, by = c("NAICS_Code", "seasonal_adjustment")) %>%
    # Reindex to make December 2024 = 100
    mutate(
      Value = (Value / dec_2024_value) * 100,
      Units = "Index, Dec 2024 = 100"
    ) %>%
    select(-dec_2024_value)  # Remove the temporary column
  
  # Final selection with the new Units column
  PPI_tidy <- bls_data_reindexed %>%
    select(
      Date, Dataset, Indicator, NAICS_Name, NAICS_Code, Units, seasonal_adjustment, Value
    )
  
}

# M3 Manufacturing Ingestion
{

  # Import Dictionaries
  data_type_dict <- read_excel("M3 data type dict.xlsx")
  naics_dict <- read_excel("M3 NAICS Dictionary.xlsx")
  naics_name <- read_csv("naics dictionary.csv")

  # Define the API endpoint URL
  api_url <- "https://api.census.gov/data/timeseries/eits/m3"
  
  # Function to fetch data for a single year
  fetch_year_data <- function(year) {
    params <- list(
      get = "data_type_code,time_slot_id,seasonally_adj,category_code,cell_value,error_data",
      `for` = "us:*",
      time = as.character(year)
    )
    
    response <- GET(url = api_url, query = params)
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
      df <- as.data.frame(data)
      colnames(df) <- df[1, ]
      df <- df[-1, ]
      rownames(df) <- NULL
      return(df)
    } else {
      warning(paste("Failed to retrieve data for year", year, ". Status code:", status_code(response)))
      return(NULL)
    }
  }
  
  # Define the range of years you want to fetch
  years <- 1992:2024  # Adjust this range as needed
  
  # Fetch data for all years
  all_data <- lapply(years, fetch_year_data)
  
  # Combine all the dataframes into one
  combined_df <- bind_rows(all_data)
  
  # Assuming your combined dataframe is named 'combined_df'
  combined_df <- combined_df %>%
    mutate(
      # Convert the time column to a proper date
      Date = ymd(paste0(time, "-01")),
      
      # Rename seasonally_adj to seasonal_adjustment and convert to logical
      seasonal_adjustment = case_when(
        seasonally_adj == "yes" ~ TRUE,
        seasonally_adj == "no" ~ FALSE,
        TRUE ~ NA  # This handles any unexpected values
      )
    ) %>%
    mutate(cell_value = as.numeric(cell_value)) %>%
    select(-seasonally_adj, -time_slot_id, -us, -error_data, -time)  # Remove the original seasonally_adj column
  
  # Perform the inner join
  combined_df <- combined_df %>%
    inner_join(data_type_dict, by = "data_type_code")
  
  # Create the new aggregates
  new_aggregates <- combined_df %>%
    group_by(Date, data_type_code, seasonal_adjustment) %>%
    summarise(
      `34N` = sum(cell_value[category_code %in% c("34D", "34E")], na.rm = TRUE),
      `34P` = sum(cell_value[category_code %in% c("34A", "34B", "34C")], na.rm = TRUE),
      `34R` = sum(cell_value[category_code %in% c("34I", "34J", "34K")], na.rm = TRUE),
      `34T` = sum(cell_value[category_code %in% c("34I", "34J")], na.rm = TRUE),
      AAP = sum(cell_value[category_code %in% c("NAP", "DAP")], na.rm = TRUE),
      `36W` = sum(cell_value[category_code %in% c("36A", "36B", "36C")], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    pivot_longer(cols = c("34N", "34P", "34R", "34T", "AAP", "36W"),
                 names_to = "category_code",
                 values_to = "cell_value") %>%
    filter(cell_value != 0)  # Remove any rows where the aggregate is zero
  
  # Combine the new aggregates with the original data
  combined_df <- bind_rows(combined_df, new_aggregates) %>%
    arrange(Date, data_type_code, category_code, seasonal_adjustment)
  
  # Perform the inner join
  combined_df <- combined_df %>%
    inner_join(naics_dict, by = "category_code")
  
  # Rename the columns to match your description
  naics_name <- naics_name %>%
    rename(NAICS_Code = code, NAICS_Name = name) %>%
    mutate(NAICS_Code = as.character(NAICS_Code))
  
  # Assuming your current dataframe has a column named 'NAICS_Code'
  # If it's named differently, replace 'NAICS_Code' with the actual column name
  combined_df <- combined_df %>%
    mutate(NAICS_Code = as.character(NAICS_Code)) %>%
    left_join(naics_name, by = "NAICS_Code") %>%
    mutate(Dataset = "M3 Manufacturers Shipments, Inventories & Orders") %>%
    rename(Value = cell_value) %>%
    select(Date, Dataset, Indicator, NAICS_Name, NAICS_Code, Units, seasonal_adjustment, Value) %>%
    mutate(NAICS_Code = as.character(NAICS_Code))
  
  # Convert "thousands of dollars" to "dollars" by multiplying by 1000
  combined_df <- combined_df %>%
    mutate(
      Value = ifelse(Units == "Thousands of Dollars", Value * 1000, Value),
      Units = ifelse(Units == "Thousands of Dollars", "Dollars", Units)
    )
  
  m3_tidy <- combined_df
  
}

# All the computed 
{

#Binding the Supply Chain Monitor Dataset
data <- bind_rows(
  Imports_tidy %>% mutate(NAICS_Code = as.character(NAICS_Code)),
  Exports_tidy %>% mutate(NAICS_Code = as.character(NAICS_Code)),
  Balance_tidy %>% mutate(NAICS_Code = as.character(NAICS_Code)),
  G17_tidy %>% mutate(NAICS_Code = as.character(NAICS_Code)),
  IPI_tidy %>% mutate(NAICS_Code = as.character(NAICS_Code)),
  EPI_tidy %>% mutate(NAICS_Code = as.character(NAICS_Code)),
  PPI_tidy %>% mutate(NAICS_Code = as.character(NAICS_Code)),
  CES_tidy %>% mutate(NAICS_Code = as.character(NAICS_Code)),
  m3_tidy %>% mutate(NAICS_Code = as.character(NAICS_Code)),
  Investment_tidy %>% mutate(NAICS_Code = as.character(NAICS_Code))
)
rm(list = setdiff(ls(), "data"))


######### Just have to do the manipulations here manually to check that they all work, 
######### but they all basically should once all the types are matched!

############### Adding infl-adj imports
# Filter the data for "Nominal Imports" and "Producer Price Index"
nominal_imports <- data %>% filter(Indicator == "Imports")
ppi <- data %>% filter(Indicator == "Producer Price Index")

# Merge the data frames based on NAICS_Code and Date
merged_data <- merge(nominal_imports, ppi, by = c("NAICS_Code", "Date"), suffixes = c("_Nominal", "_PPI"))

# Calculate the "Inflation-Adjusted Imports"
merged_data <- merged_data %>%
  mutate(Inflation_Adjusted_Imports = Value_Nominal / (Value_PPI/100))

# Select only the necessary columns
result <- merged_data %>%
  select(Date, Dataset_Nominal, NAICS_Name_Nominal, NAICS_Code, seasonal_adjustment_Nominal, Inflation_Adjusted_Imports) %>%
  mutate(Indicator = "PPI-Deflated Imports",
         Units = "Dollars, PPI Indexed to Dec 2024"
        ) %>%
  rename(Value = Inflation_Adjusted_Imports,
         seasonal_adjustment = seasonal_adjustment_Nominal,
         Dataset = Dataset_Nominal,
         NAICS_Name = NAICS_Name_Nominal
         ) %>%
  select(Date, Dataset, Indicator, NAICS_Name, Units, NAICS_Code, seasonal_adjustment, Value)
data <- rbind(data,result)

############# Taking a look at how different IPI-adjusted imports are
# Filter the data for "Nominal Imports" and "Import Price Index"
nominal_imports <- data %>% filter(Indicator == "Imports")
ipi <- data %>% filter(Indicator == "Import Price Index")

# Merge the data frames based on NAICS_Code and Date
merged_data <- merge(nominal_imports, ipi, by = c("NAICS_Code", "Date"), suffixes = c("_Nominal", "_IPI"))

# Calculate the "Inflation-Adjusted Imports"
merged_data <- merged_data %>%
  mutate(Inflation_Adjusted_Imports = Value_Nominal / (Value_IPI/100))

# Select only the necessary columns
result <- merged_data %>%
  select(Date, Dataset_Nominal, NAICS_Name_Nominal, NAICS_Code, seasonal_adjustment_Nominal, Inflation_Adjusted_Imports) %>%
  mutate(Indicator = "IPI-Deflated Imports",
         Units = "Dollars, IPI Indexed to Dec 2024") %>%
  rename(Value = Inflation_Adjusted_Imports,
         seasonal_adjustment = seasonal_adjustment_Nominal,
         Dataset = Dataset_Nominal,
         NAICS_Name = NAICS_Name_Nominal
  ) %>%
  select(Date, Dataset, Indicator, NAICS_Name, NAICS_Code, Units, seasonal_adjustment, Value)
data <- rbind(data,result)

############### Adding infl-adj exports
# Filter the data for "Nominal Exports" and "Producer Price Index"
nominal_exports <- data %>% filter(Indicator == "Exports")
ppi <- data %>% filter(Indicator == "Producer Price Index")

# Merge the data frames based on NAICS_Code and Date
merged_data <- merge(nominal_exports, ppi, by = c("NAICS_Code", "Date"), suffixes = c("_Nominal", "_PPI"))

# Calculate the "Inflation-Adjusted Exports"
merged_data <- merged_data %>%
  mutate(Inflation_Adjusted_Exports = Value_Nominal / (Value_PPI/100))

# Select only the necessary columns
result <- merged_data %>%
  select(Date, Dataset_Nominal, NAICS_Name_Nominal, NAICS_Code, seasonal_adjustment_Nominal, Inflation_Adjusted_Exports) %>%
  mutate(Indicator = "PPI-Adjusted Exports",
         Units = "Dollars, PPI Indexed to Dec 2024") %>%
  rename(Value = Inflation_Adjusted_Exports,
         seasonal_adjustment = seasonal_adjustment_Nominal,
         Dataset = Dataset_Nominal,
         NAICS_Name = NAICS_Name_Nominal
  ) %>%
  select(Date, Dataset, Indicator, NAICS_Name, NAICS_Code, Units, seasonal_adjustment, Value)
data <- rbind(data,result)

############### Adding infl-adj exports
# Filter the data for "Nominal Exports" and "Export Price Index"
nominal_exports <- data %>% filter(Indicator == "Exports")
epi <- data %>% filter(Indicator == "Export Price Index")

# Merge the data frames based on NAICS_Code and Date
merged_data <- merge(nominal_exports, epi, by = c("NAICS_Code", "Date"), suffixes = c("_Nominal", "_EPI"))

# Calculate the "Inflation-Adjusted Exports"
merged_data <- merged_data %>%
  mutate(Inflation_Adjusted_Exports = Value_Nominal / (Value_EPI/100))

# Select only the necessary columns
result <- merged_data %>%
  select(Date, Dataset_Nominal, NAICS_Name_Nominal, NAICS_Code, seasonal_adjustment_Nominal, Inflation_Adjusted_Exports) %>%
  mutate(Indicator = "EPI-Adjusted Exports",
         Units = "Dollars, EPI Indexed to Dec 2024") %>%
  rename(Value = Inflation_Adjusted_Exports,
         seasonal_adjustment = seasonal_adjustment_Nominal,
         Dataset = Dataset_Nominal,
         NAICS_Name = NAICS_Name_Nominal
  ) %>%
  select(Date, Dataset, Indicator, NAICS_Name, NAICS_Code, Units, seasonal_adjustment, Value)
data <- rbind(data,result)

data <- data %>%
  distinct(Date, NAICS_Code, NAICS_Name, Indicator, seasonal_adjustment, .keep_all = TRUE)

# Create the new index column
data$index_col <- paste(data$NAICS_Code, "-", data$NAICS_Name)
}
# Add Axis_Type column and Combined column to prevent issues
data$Axis_Type <- as.character("")

data$Combined = as.character("")

data$Date <- as.Date(data$Date)

# Remove rows where NAICS_Code matches any value in the list
data <- data %>%
  filter(!(NAICS_Code %in% c("31", "32", "33", "NA", "MANU", "NONMAN", 
                             "329", "3291", "32911", "329111", "32914", "3299",
                             "911", "91622", "91911", "91912", "919999", 
                             "921611", "922622", "92292", "922999", "931611",
                             "932", "932221", "93248", "932622", "93292", "932999")))

# Original .RData format for existing code
save(data, file = "data_index.RData")
