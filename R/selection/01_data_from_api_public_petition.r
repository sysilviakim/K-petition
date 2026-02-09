## Getting Data from OpenAPI
## API key for public_petition
## data.go.kr -- make your own account and get the API key
## Decoding Key works -- error in documentation

library(tidyverse)
library(httr)
library(jsonlite)
library(xml2)

## First, collect "petiNo" from the public_petition API
## Then, get detailed information about the petition using the "petiNo"

# Loop through each year from 2002 to 2024
for (year in 2002:2024) {
  # Set up the parameters for the current year
  regFrom <- paste0(year, "0101") # Start of the year (YYYYMMDD)
  regTo <- paste0(year, "1231") # End of the year (YYYYMMDD)

  params <- list(
    serviceKey = api_key, # API Key (required)
    regFrom = regFrom, # Registration start date for the current year
    regTo = regTo, # Registration end date for the current year
    firstIndex = 1, # Current page number
    # default is 10 but can be set to a large number
    recordCountPerPage = 9999999
  )

  # Make the GET request with the defined parameters
  response <- GET(
    url = api_url,
    query = params
  )

  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON response directly
    data <- content(response, "text", encoding = "UTF-8")
    json_data <- fromJSON(data, flatten = TRUE)

    # Check if 'resultList' exists in the data
    if (!is.null(json_data$resultList)) {
      # Access the 'resultList' and convert it to a dataframe
      df <- as.data.frame(json_data$resultList)

      # Select only the specified columns
      df <- df %>%
        select(
          petiNo,
          title,
          regDate,
          statusName,
          statusCode,
          ancName
        )

      # Define the filename for the current year
      file_name <- paste0("public_petition_data_", year, ".csv")

      # Save the selected columns to a CSV file
      write_csv(df, file_name)

      # Print a message to confirm the file was saved
      message("Data for year ", year, " saved to ", file_name)
    } else {
      message("No data available for year ", year)
    }
  } else {
    # Print the error message if the request failed
    message("Error:", status_code(response), " for year ", year)
  }
}

#############################
## error for 2020, 2023, 2024
#############################

# Non-JSON response for 2020-07
# Non-JSON response for 2023-02
# Non-JSON response for 2024-02
# check JSON format for 2024

# still working on this - kyusik
# HTTP ROUTING ERROR -- common for data.go.kr API
