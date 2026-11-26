# 02b_smoke_stations.R
# Air chapter: daily PM2.5 at stations down the Fraser in 2018, plus
# Port Moody for 2017 to 2019 (for the synthetic pregnancy exposure lab).
#
# Source: BC Ministry of Environment and Parks, Air Quality and Climate Monitoring:
# Verified Hourly Air Quality and Meteorological Data.
# Licence: Open Government Licence - British Columbia
#
# Daily mean = mean of the hourly values for that date, kept only when at least
# 18 of the 24 hours have a value (the usual 75% completeness rule).
#
# Run from the two-rivers RStudio project. Needs 02a_smoke_explore.R to have
# downloaded the 2018 file first.
# Outputs:
#   data/processed/smoke_2018_stations.json
#   data/processed/pm25_port_moody_2017_2019.json

library(readr)
library(dplyr)
library(here)
library(jsonlite)

options(timeout = 1800)
base_url <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/"

# Stations from source to sea (edit this list to change them)
river_stations <- c(
  "Valemount",
  "Prince George Plaza 400",
  "Quesnel Senior Secondary",
  "Williams Lake Columneetza School",
  "Kamloops Federal Building",
  "Hope Airport",
  "Abbotsford Central",
  "Port Moody Rocky Point Park",
  "Vancouver International Airport #2"
)
lab_station <- "Port Moody Rocky Point Park"

# ---- Helpers ----
get_year <- function(year) {
  dest <- here("data", "raw", paste0("bc_air_", year, "_PM25.csv"))
  if (!file.exists(dest)) {
    download.file(paste0(base_url, year, "/PM25.csv"), dest, mode = "wb")
  }
  read_csv(dest, show_col_types = FALSE, guess_max = 100000)
}

daily_means <- function(hourly) {
  hourly |>
    group_by(EMS_ID, STATION_NAME, date = DATE) |>
    summarise(hours_valid = sum(!is.na(RAW_VALUE)),
              pm25 = mean(RAW_VALUE, na.rm = TRUE),
              .groups = "drop") |>
    mutate(pm25 = if_else(hours_valid >= 18, round(pm25, 1), NA_real_))
}

# ---- 1. 2018, river stations ----
pm18 <- get_year(2018)
missing <- setdiff(river_stations, unique(pm18$STATION_NAME))
if (length(missing) > 0) stop("Not found in the 2018 file: ", paste(missing, collapse = ", "))

river_daily <- pm18 |>
  filter(STATION_NAME %in% river_stations) |>
  daily_means() |>
  mutate(STATION_NAME = factor(STATION_NAME, levels = river_stations)) |>
  arrange(STATION_NAME, date)

# Station locations
stn_dest <- here("data", "raw", "bc_air_monitoring_stations.csv")
if (!file.exists(stn_dest)) {
  download.file(paste0(base_url, "2018/bc_air_monitoring_stations.csv"), stn_dest, mode = "wb")
}
stn_all <- read_csv(stn_dest, show_col_types = FALSE)
lat_col <- grep("^LAT", names(stn_all), value = TRUE, ignore.case = TRUE)[1]
lon_col <- grep("^LON", names(stn_all), value = TRUE, ignore.case = TRUE)[1]
stn <- stn_all |>
  filter(EMS_ID %in% unique(river_daily$EMS_ID)) |>
  distinct(EMS_ID, .keep_all = TRUE) |>
  transmute(EMS_ID, lat = .data[[lat_col]], lon = .data[[lon_col]])

stations_meta <- river_daily |>
  distinct(STATION_NAME, EMS_ID) |>
  left_join(stn, by = "EMS_ID") |>
  mutate(order = as.integer(STATION_NAME), name = as.character(STATION_NAME)) |>
  select(order, name, ems_id = EMS_ID, lat, lon)

# ---- 2. Checks ----
message("\n--- 2018 checks (days with a valid daily mean, highest day) ---")
checks <- river_daily |>
  group_by(STATION_NAME) |>
  summarise(days_valid = sum(!is.na(pm25)),
            jul_sep_valid = sum(!is.na(pm25) & date >= as.Date("2018-07-01") & date <= as.Date("2018-09-30")),
            max_daily = max(pm25, na.rm = TRUE),
            max_date = date[which.max(pm25)],
            .groups = "drop")
print(checks, n = Inf, width = Inf)
message("Stations without a location: ", sum(is.na(stations_meta$lat)))

# ---- 3. Save the river file (one array of values per station, in date order) ----
dates18 <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day")
values <- lapply(river_stations, function(s) {
  d <- river_daily |> filter(STATION_NAME == s)
  d$pm25[match(dates18, d$date)]
})
names(values) <- river_stations

write_json(
  list(
    meta = list(
      year = 2018,
      unit = "ug/m3",
      method = "Daily mean of verified hourly PM2.5, kept when at least 18 of 24 hours have a value.",
      source = "BC Ministry of Environment and Parks, Verified Hourly Air Quality and Meteorological Data",
      licence = "Open Government Licence - British Columbia"
    ),
    stations = stations_meta,
    dates = format(dates18, "%Y-%m-%d"),
    values = values
  ),
  here("data", "processed", "smoke_2018_stations.json"),
  auto_unbox = TRUE, na = "null", digits = NA
)
message("Saved: data/processed/smoke_2018_stations.json")

# ---- 4. Port Moody, 2017 to 2019, for the pregnancy lab ----
lab_id <- stations_meta$ems_id[stations_meta$name == lab_station]
lab_hourly <- bind_rows(
  get_year(2017) |> filter(EMS_ID == lab_id),
  pm18 |> filter(EMS_ID == lab_id),
  get_year(2019) |> filter(EMS_ID == lab_id)
) |>
  select(EMS_ID, STATION_NAME, DATE, RAW_VALUE)

lab_daily <- lab_hourly |> daily_means() |> arrange(date)

message("\n--- Port Moody checks ---")
print(lab_daily |>
        group_by(year = format(date, "%Y")) |>
        summarise(days_valid = sum(!is.na(pm25)), max_daily = max(pm25, na.rm = TRUE)),
      width = Inf)

lab_dates <- seq(as.Date("2017-01-01"), as.Date("2019-12-31"), by = "day")
write_json(
  list(
    meta = list(
      station = lab_station,
      ems_id = lab_id,
      unit = "ug/m3",
      method = "Daily mean of verified hourly PM2.5, kept when at least 18 of 24 hours have a value.",
      source = "BC Ministry of Environment and Parks, Verified Hourly Air Quality and Meteorological Data",
      licence = "Open Government Licence - British Columbia",
      note = "Real air quality data. Any pregnancies shown with it in the story are made up."
    ),
    dates = format(lab_dates, "%Y-%m-%d"),
    pm25 = lab_daily$pm25[match(lab_dates, lab_daily$date)]
  ),
  here("data", "processed", "pm25_port_moody_2017_2019.json"),
  auto_unbox = TRUE, na = "null", digits = NA
)
message("Saved: data/processed/pm25_port_moody_2017_2019.json")
