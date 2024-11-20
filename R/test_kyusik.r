# Load necessary libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(xml2)

# Define the API endpoint and key
api_url <- "http://apis.data.go.kr/1140100/OpenProposalService2/OpenProposalList"
api_key <- "Gn4r43PDuekpvZV8ULhDM7Xw63ZAz6ASLvPHKYK4FCN0d2lV+UFtCqNd2bG+HVi9O9gXeJj1R+mCiDwpEutc/g=="

# Function to fetch and save data by month for a given year
fetch_data_by_month <- function(year) {
    # Initialize an empty dataframe to store the combined data
    combined_df <- data.frame()

    # Loop through each month
    for (month in 1:12) {
        # Format month as two digits
        month_str <- sprintf("%02d", month)

        # Set up parameters for the current month
        regFrom <- paste0(year, month_str, "01")
        regTo <- paste0(year, month_str, sprintf("%02d", days_in_month(as.Date(paste0(year, "-", month, "-01")))))

        params <- list(
            serviceKey = api_key,
            regFrom = regFrom,
            regTo = regTo,
            firstIndex = 1,
            recordCountPerPage = 9999999 # Set to a reasonable limit per month
        )

        # Make the GET request
        response <- GET(url = api_url, query = params)

        # Check if the request was successful
        if (status_code(response) == 200) {
            data <- content(response, "text", encoding = "UTF-8")

            # Check if the response is JSON
            if (http_type(response) == "application/json") {
                json_data <- fromJSON(data, flatten = TRUE)

                # Check if 'resultList' exists in the JSON data
                if (!is.null(json_data$resultList)) {
                    # Convert the 'resultList' to a dataframe
                    df <- as.data.frame(json_data$resultList) %>%
                        select(
                            petiNo,
                            title,
                            regDate,
                            statusName,
                            statusCode,
                            ancName
                        )

                    # Combine with the main dataframe for the year
                    combined_df <- bind_rows(combined_df, df)
                }
            } else {
                # Handle XML errors, if any
                message("Non-JSON response for ", year, "-", month_str)
            }
        } else {
            message("Error:", status_code(response), " for ", year, "-", month_str)
        }
    }

    # Save the combined data for the year if any data was collected
    if (nrow(combined_df) > 0) {
        file_name <- paste0("public_petition_data_", year, ".csv")
        write_csv(combined_df, file_name)
        message("Data for year ", year, " saved to ", file_name)
    } else {
        message("No data collected for year ", year)
    }
}

# Main script to collect data
for (year in 2002:2024) {
    if (year %in% c(2020, 2023, 2024)) {
        # For 2020, 2023, and 2024, fetch data by month
        message("Collecting data by month for ", year)
        fetch_data_by_month(year)
    } else {
        # Set up parameters for the full year for other years
        regFrom <- paste0(year, "0101")
        regTo <- paste0(year, "1231")

        params <- list(
            serviceKey = api_key,
            regFrom = regFrom,
            regTo = regTo,
            firstIndex = 1,
            recordCountPerPage = 99999999
        )

        # Make the GET request
        response <- GET(url = api_url, query = params)

        # Check if the request was successful
        if (status_code(response) == 200) {
            data <- content(response, "text", encoding = "UTF-8")

            # Check if the response is JSON
            if (http_type(response) == "application/json") {
                json_data <- fromJSON(data, flatten = TRUE)

                # Check if 'resultList' exists in the JSON data
                if (!is.null(json_data$resultList)) {
                    # Convert the 'resultList' to a dataframe
                    df <- as.data.frame(json_data$resultList) %>%
                        select(
                            petiNo,
                            title,
                            regDate,
                            statusName,
                            statusCode,
                            ancName
                        )

                    # Save the data for the full year
                    file_name <- paste0("public_petition_data_", year, ".csv")
                    write_csv(df, file_name)
                    message("Data for year ", year, " saved to ", file_name)
                } else {
                    message("No data available for year ", year)
                }
            } else {
                message("Non-JSON response for year ", year)
            }
        } else {
            message("Error:", status_code(response), " for year ", year)
        }
    }
}

#############################
## error for 2020, 2023, 2024
#############################

# Non-JSON response for 2020-07
# Non-JSON response for 2023-02
# Non-JSON response for 2024-02
# check JSON format for 2024

# Simplified parameters for testing
params <- list(
    serviceKey = api_key, # API Key (required)
    regFrom = "20200101", # Start date for testing (YYYYMMDD)
    regTo = "20201231", # End date for testing (YYYYMMDD)
    firstIndex = 1, # Page number
    recordCountPerPage = 1000 #
)

# Make the GET request with the defined parameters
response <- GET(
    url = api_url,
    query = params
)

#### working on this
