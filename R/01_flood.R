# 01_flood.R
# Flood chapter: the November 2021 atmospheric river and Sumas Prairie.
#
# Gets daily river data for two gauges and saves one small JSON file
# for the story page:
#   - Sumas River near Huntingdon, BC (08MH029), Water Survey of Canada (HYDAT)
#   - Nooksack River at North Cedarville, WA (USGS-12210700), US Geological Survey
# Most of the water that flooded Sumas Prairie came from the Nooksack
# overflowing at Everson, so the story shows both gauges.
#
# Licences:
#   - HYDAT: Open Government Licence - Canada
#   - USGS: public domain (US federal government data)
#
# Run from the two-rivers RStudio project. Output: data/processed/flood_2021.json

library(tidyhydat)
library(dataRetrieval)
library(dplyr)
library(here)
library(jsonlite)

start_date <- "2021-11-01"
end_date   <- "2021-12-15"

# ---- 1. HYDAT (downloads once, about 1 GB, can take 10 minutes or more) ----
if (!file.exists(hy_downloaded_db())) {
  download_hydat(ask = FALSE)
}
hydat_version <- hy_version()
message("HYDAT version: ", format(hydat_version$Date[1], "%Y-%m-%d"))

station <- hy_stations(station_number = "08MH029")
message("Canadian gauge: ", station$STATION_NAME, " (", station$STATION_NUMBER, ")")

sumas_level <- hy_daily_levels(station_number = "08MH029",
                               start_date = start_date, end_date = end_date) |>
  transmute(date = Date, sumas_level_m = round(Value, 2), sumas_level_flag = Symbol)

sumas_flow <- hy_daily_flows(station_number = "08MH029",
                             start_date = start_date, end_date = end_date) |>
  transmute(date = Date, sumas_flow_m3s = round(Value, 1), sumas_flow_flag = Symbol)

# ---- 2. USGS Nooksack (no download needed) ----
nooksack_site <- read_waterdata_monitoring_location(monitoring_location_id = "USGS-12210700")
message("US gauge: ", nooksack_site$monitoring_location_name, " (USGS-12210700)")

nooksack <- read_waterdata_daily(monitoring_location_id = "USGS-12210700",
                                 parameter_code = "00060",   # discharge, cubic feet per second
                                 statistic_id = "00003",     # daily mean
                                 time = c(start_date, end_date),
                                 skipGeometry = TRUE) |>
  transmute(date = as.Date(time),
            nooksack_flow_m3s = round(value * 0.0283168, 1),  # cubic feet to cubic metres
            nooksack_approval = approval_status)

# ---- 3. Join and check ----
days <- tibble(date = seq(as.Date(start_date), as.Date(end_date), by = "day")) |>
  left_join(sumas_level, by = "date") |>
  left_join(sumas_flow, by = "date") |>
  left_join(nooksack, by = "date")

message("\n--- Checks ---")
message("Days in window: ", nrow(days))
message("Missing Sumas level days: ", sum(is.na(days$sumas_level_m)))
message("Missing Sumas flow days: ", sum(is.na(days$sumas_flow_m3s)))
message("Missing Nooksack flow days: ", sum(is.na(days$nooksack_flow_m3s)))
message("Highest Sumas daily level: ", max(days$sumas_level_m, na.rm = TRUE), " m on ",
        days$date[which.max(days$sumas_level_m)])
message("Highest Nooksack daily flow: ", max(days$nooksack_flow_m3s, na.rm = TRUE), " m3/s on ",
        days$date[which.max(days$nooksack_flow_m3s)])
message("Sumas flags used: ", paste(unique(na.omit(c(days$sumas_level_flag, days$sumas_flow_flag))), collapse = ", "))
message("Nooksack approval: ", paste(unique(na.omit(days$nooksack_approval)), collapse = ", "))

# ---- 4. Save for the story page ----
out <- list(
  meta = list(
    window = c(start_date, end_date),
    hydat_version = format(hydat_version$Date[1], "%Y-%m-%d"),
    note = paste("Daily means. Instantaneous peaks were higher than these daily values.",
                 "Sumas River flow is missing in HYDAT on some peak days; Sumas River level is complete.",
                 "Flag E means estimated (Water Survey of Canada)."),
    sources = list(
      list(name = "Water Survey of Canada, HYDAT, station 08MH029 (Sumas River near Huntingdon)",
           licence = "Open Government Licence - Canada"),
      list(name = "U.S. Geological Survey, station 12210700 (Nooksack River at North Cedarville, WA)",
           licence = "Public domain")
    )
  ),
  days = days |> mutate(date = format(date, "%Y-%m-%d"))
)

dir.create(here("data", "processed"), showWarnings = FALSE, recursive = TRUE)
write_json(out, here("data", "processed", "flood_2021.json"),
           auto_unbox = TRUE, pretty = TRUE, na = "null", digits = NA)
message("\nSaved: data/processed/flood_2021.json")
