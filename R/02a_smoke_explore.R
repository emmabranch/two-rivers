# 02a_smoke_explore.R
# Air chapter, first look: what is in BC's verified 2018 PM2.5 file?
# Changes nothing in the story. Prints the folder listing, the column names,
# and every station with how many hours it reported in 2018.
#
# Source: BC Ministry of Environment and Parks, Air Quality and Climate Monitoring:
# Verified Hourly Air Quality and Meteorological Data.
# Licence: Open Government Licence - British Columbia
#
# Run from the two-rivers RStudio project.

library(readr)
library(dplyr)
library(here)

options(timeout = 1800)  # big file: allow up to 30 minutes

year_url <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/2018/"

# ---- 1. What files are in the 2018 folder? ----
listing <- tempfile()
download.file(year_url, listing, quiet = TRUE)
message("--- Files in the 2018 folder ---")
cat(readLines(listing), sep = "\n")

# ---- 2. Download the PM2.5 file once (kept in data/raw, not sent to GitHub) ----
dest <- here("data", "raw", "bc_air_2018_PM25.csv")
if (!file.exists(dest)) {
  download.file(paste0(year_url, "PM25.csv"), dest, mode = "wb")
}
message("\nFile size: ", round(file.size(dest) / 1e6, 1), " MB")

# ---- 3. What's inside? ----
pm <- read_csv(dest, show_col_types = FALSE, guess_max = 100000)
message("\n--- Columns ---")
print(names(pm))
message("\n--- First rows ---")
print(head(pm, 3), width = Inf)

# ---- 4. Every station and how many hours it reported ----
if ("STATION_NAME" %in% names(pm)) {
  value_col <- intersect(c("RAW_VALUE", "ROUNDED_VALUE", "VALUE"), names(pm))[1]
  stations <- pm |>
    group_by(STATION_NAME) |>
    summarise(hours = n(),
              hours_with_value = sum(!is.na(.data[[value_col]])),
              .groups = "drop") |>
    arrange(STATION_NAME)
  message("\n--- Stations (value column used: ", value_col, ") ---")
  print(stations, n = Inf)
} else {
  message("\nNo STATION_NAME column: paste the column list above back to Claude.")
}
