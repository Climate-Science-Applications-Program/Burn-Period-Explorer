###############################################################################
# Script: get_RAWS_rh.R
# Purpose: Download hourly RAWS observations from the FEMS Climatology API,
#          join to local station metadata, convert timestamps to local time,
#          and return a tidy data frame with temp/RH fields for use in Shiny.
#
# Author: Michael A. Crimmins (UofA CSAP)     Contact: crimmins@arizona.edu
# Last updated: 2025-07-31 (America/Phoenix)
#
# Overview:
# - Issues an HTTP GET to:
#       https://fems.fs2c.usda.gov/api/climatology/download-weather
# - Requests CSV output for a station and date range at a chosen increment.
# - Reads and cleans the CSV, joins to `locations_df` for tz/lat/lon metadata.
# - Converts UTC timestamps to the station’s local timezone (from metadata).
# - Returns a compact tibble with date, hour, temperature (F), and RH (%).
#
# Why these choices:
# - Browser-like headers (User-Agent, Referer, Origin) are added to the request
#   to avoid 403 responses from the upstream load balancer/WAF.
# - Time handling avoids mixed-timezone POSIXct vectors; we convert once from
#   UTC -> target tz and derive `obs_dt` (Date) and `obs_tm` (0–23 hour).
#
# Dependencies:
#   httr, readr, dplyr, lubridate, janitor
#
# Inputs:
#   stationID   : character or integer; RAWS station identifier accepted by FEMS.
#   locations_df: data frame with at least columns:
#                 - station_id (chr/int)
#                 - station_name (chr)
#                 - latitude (dbl)
#                 - longitude (dbl)
#                 - tz (chr; valid Olson name, e.g., "America/Phoenix")
#   startDate   : character or Date; YYYY-MM-DD (UTC midnight assumed).
#   endDate     : character or Date; YYYY-MM-DD (UTC 23:59:59 assumed).
#   increment   : character; usually "hourly".
#
# Returns:
#   tibble with columns:
#     - sta_id   (station_id from locations_df)
#     - sta_nm   (station_name)
#     - latitude (numeric)
#     - longitude(numeric)
#     - obs_dt   (Date in local tz)
#     - obs_tm   (integer hour 0–23 in local tz)
#     - dry_temp (numeric; F)
#     - rh       (numeric; %)
#
# Expected CSV fields from API (clean/names lowercased by janitor::clean_names):
#   - date_time (POSIXct, UTC), station_name, temperature_f, relative_humidity_percent
#
# Usage (example):
#   locations <- read_csv("stations_metadata.csv")
#   station_id <- 21202
#   df <- get_RAWS_rh(station_id, locations,
#                     startDate = "2025-07-01", endDate = Sys.Date())
#
# Notes / Caveats:
# - If you see HTTP 403, ensure:
#     * The function is using the browser-like headers below,
#     * The requested date range is allowed for unauthenticated access,
#       or use a FEMS “Copy Data Link” (CDL) URL workflow for historical spans.
# - `stationtypes` query param is omitted by default; re-enable only if needed.
# - `America/Phoenix` does not observe DST; other time zones may.
#
# Change log:
# - 2025-07-31: Added browser-like headers to mitigate 403; simplified TZ logic
#               to avoid mixed-tz POSIXct and removed `format()`-based date parse.
###############################################################################

library(httr)
library(readr)
library(dplyr)
library(lubridate)
library(janitor)

# Define the function
get_RAWS_rh <- function(stationID, 
                        locations_df,
                        startDate = "2000-01-01", 
                        endDate   = Sys.Date(),
                        increment = "hourly") {
  
  # Format dates into ISO 8601 with Zulu time
  startDate_formatted <- paste0(startDate, "T00:00:00Z")
  endDate_formatted   <- paste0(endDate,   "T23:59:59Z")
  
  # Build the API URL
  base_url <- "https://fems.fs2c.usda.gov/api/climatology/download-weather"
  query <- list(
    stationIds    = stationID,
    startDate     = startDate_formatted,
    endDate       = endDate_formatted,
    dataset       = "observation",
    dataFormat    = "csv",
    dataIncrement = increment
    # , stationtypes = "RAWS(SATNFDRS)"  # often unnecessary; comment out to reduce risk of 4xx
  )
  
  # Perform the GET request with browser-like headers (helps avoid 403/WAF)
  ua <- httr::user_agent(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/127.0.0.0 Safari/537.36"
  )
  response <- httr::RETRY(
    "GET", base_url, query = query, ua,
    httr::add_headers(
      Referer = "https://fems.fs2c.usda.gov/download",
      Origin  = "https://fems.fs2c.usda.gov",
      `Accept` = "text/csv,application/octet-stream;q=0.9,*/*;q=0.8",
      `Accept-Language` = "en-US,en;q=0.9"
    ),
    times = 3, pause_min = 1, pause_cap = 4
  )
  
  # Check if the request was successful
  if (status_code(response) != 200) {
    stop("Failed to download data: HTTP ", status_code(response),
         ". First 200 chars: ", substr(httr::content(response, "text"), 1, 200))
  }
  
  # Read the CSV content directly into a dataframe
  df <- read_csv(content(response, as = "text"), show_col_types = FALSE) %>%
    clean_names() %>%
    filter(!is.na(date_time)) %>%
    # Join on station_name (matches your locations_df columns)
    left_join(locations_df, by = c("station_name" = "name"))
  
  # --- Time zone handling ---
  # date_time is UTC per CSV. Convert once to the joined tz (America/Phoenix here).
  # Avoid creating a mixed‑TZ POSIXct vector and avoid format(); use as.Date/hour() directly.
  tz_out <- unique(df$tz)
  tz_out <- tz_out[!is.na(tz_out)]
  if (length(tz_out) == 0) tz_out <- "UTC" else tz_out <- tz_out[1]
  
  df <- df %>%
    mutate(
      local_time = with_tz(force_tz(date_time, tzone = "UTC"), tz_out),
      obs_dt = as.Date(local_time),
      obs_tm = hour(local_time)
    )
  
  # Select and rename columns to match desired output (align to locations_df names)
  df <- df %>%
    transmute(
      #sta_id   = station_id,
      sta_id   = StationID,
      sta_nm   = station_name,
      latitude = lat,
      longitude= lng,
      obs_dt,
      obs_tm,
      dry_temp = temperature_f,
      rh       = relative_humidity_percent
    )
  
  return(df)
}

# Example usage:
# station_id <- 21202
# rawsData <- get_RAWS_rh(station_id, locations_df = locations)
# head(rawsData)


