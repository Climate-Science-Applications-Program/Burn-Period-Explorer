library(httr)
library(readr)
library(dplyr)
library(lubridate)
library(janitor)

# Define the function
get_RAWS_rh <- function(stationID, 
                                  locations_df,
                                  startDate = "2000-01-01", 
                                  endDate = Sys.Date() - 0,
                                  increment = "hourly") {
  
  # Format dates into ISO 8601 with Zulu time
  startDate_formatted <- paste0(startDate, "T00:00:00Z")
  endDate_formatted <- paste0(endDate, "T23:59:59Z")
  
  # Build the API URL
  base_url <- "https://fems.fs2c.usda.gov/api/climatology/download-weather"
  query <- list(
    stationIds = stationID,
    startDate = startDate_formatted,
    endDate = endDate_formatted,
    dataset = "observation",
    dataFormat = "csv",
    dataIncrement = increment,
    stationtypes = "RAWS(SATNFDRS)"
  )
  
  # Perform the GET request
  response <- GET(base_url, query = query)
  
  # Check if the request was successful
  if (status_code(response) != 200) {
    stop("Failed to download data: ", status_code(response))
  }
  
  # Read the CSV content directly into a dataframe
  df <- read_csv(content(response, as = "text"))
  
  # Basic processing: clean column names, remove any empty rows
  df <- df %>%
    clean_names() %>%
    filter(!is.na(date_time))
  
  # Merge with location metadata
  df <- df %>%
    rename(station_id = station_id, date_time = date_time) %>%
    left_join(locations_df, by = c("station_id" = "StationID"))
  
  # Convert date_time to local timezone based on metadata tz more efficiently
  df <- df %>%
    mutate(
      tz = if_else(is.na(tz), "UTC", tz),
      local_time = force_tz(date_time, tzone = "UTC")
    )
  
  # Apply timezone adjustments efficiently
  unique_tzs <- unique(df$tz)
  for (timezone in unique_tzs) {
    df <- df %>%
      mutate(local_time = if_else(tz == timezone, with_tz(local_time, tzone = timezone), local_time))
  }
  
  df <- df %>%
    mutate(obs_dt = format(local_time, "%m/%d/%Y"),
           obs_tm = hour(local_time))
  df$obs_dt<-as.Date(df$obs_dt, format = "%m/%d/%Y")
  
  # Select and rename columns to match desired output
  df <- df %>%
    transmute(
      sta_id = station_id,
      sta_nm = name,
      latitude = lat,
      longitude = lng,
      obs_dt,
      obs_tm,
      dry_temp = `temperature_f`,
      rh = `relative_humidity_percent`
    )
  
  return(df)
}
# Example usage:
# weather_data <- download_weather_data(stationID = 21202, locations_df = locations)
# head(weather_data)
#station_id <- 21202
#rawsData<-get_RAWS_rh(station_id, locations)

