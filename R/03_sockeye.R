# 03_sockeye.R
# River chapter: Fraser sockeye spawners.
#
# Uses the same cleaned NuSEDS file as the BC Salmon Explorer:
#   Carturan, B. & Peacock, S. (2026). Cleaned spawner survey data (v4).
#   Pacific Salmon Foundation. Zenodo. https://doi.org/10.5281/zenodo.19959144
#   Licence: CC BY 4.0. Derived from Fisheries and Oceans Canada NuSEDS
#   (Open Government Licence - Canada).
#
# These are counts of spawners on the spawning grounds (escapement), not total
# returns: fish caught on the way home are not included. Not every stream is
# surveyed every year, so yearly totals also reflect how many streams were counted.
#
# Looks for the file in data/raw/ first, then in the bc-salmon-explorer project.
# Run from the two-rivers RStudio project.
# Output: data/processed/sockeye_fraser.json

library(readr)
library(dplyr)
library(here)
library(jsonlite)

file_name <- "2_nuseds_cuid_streamid_2026-02-20.csv"
candidates <- c(here("data", "raw", file_name),
                here("..", "bc-salmon-explorer", "data-raw", file_name))
raw_path <- candidates[file.exists(candidates)][1]
if (is.na(raw_path)) stop("Download ", file_name, " from https://doi.org/10.5281/zenodo.19959144 into data/raw/")
message("Reading: ", raw_path)

raw <- read_csv(raw_path,
                col_select = c("SPECIES", "SPECIES_QUALIFIED", "region", "cuid", "cu_name_pse",
                               "streamid", "sys_nm", "Y_LAT", "X_LONGT", "Year", "MAX_ESTIMATE"),
                guess_max = 100000, show_col_types = FALSE)

sockeye <- raw |>
  filter(region == "Fraser",
         SPECIES_QUALIFIED %in% c("SEL", "SER"),   # lake-type and river-type sockeye
         !is.na(cuid), !is.na(streamid), !is.na(MAX_ESTIMATE))

# ---- 1. Totals by year ----
by_year <- sockeye |>
  group_by(year = as.integer(Year)) |>
  summarise(spawners = sum(MAX_ESTIMATE), streams_counted = n_distinct(streamid),
            .groups = "drop") |>
  arrange(year)

# ---- 2. 2018: one row per spawning stream (where the dots swim to) ----
streams_2018 <- sockeye |>
  filter(Year == 2018) |>
  group_by(streamid) |>
  summarise(stream = first(sys_nm), cu = first(cu_name_pse),
            lat = round(first(Y_LAT), 3), lon = round(first(X_LONGT), 3),
            spawners = sum(MAX_ESTIMATE), .groups = "drop") |>
  arrange(desc(spawners))

# ---- 3. Checks ----
message("\n--- Checks ---")
message("Years: ", min(by_year$year), " to ", max(by_year$year))
message("2018 total spawners counted: ", format(sum(streams_2018$spawners), big.mark = ","))
message("2018 streams: ", nrow(streams_2018), "; missing coordinates: ",
        sum(is.na(streams_2018$lat) | is.na(streams_2018$lon)))
message("Top 5 streams in 2018:")
print(head(streams_2018 |> select(stream, cu, spawners), 5), width = Inf)
message("Last 8 years:")
print(tail(by_year, 8))

# ---- 4. Save ----
write_json(
  list(
    meta = list(
      species = "Sockeye salmon, Fraser River (lake-type and river-type)",
      measure = "Spawners counted on the spawning grounds (escapement), not total returns",
      note = "Not every stream is surveyed every year; yearly totals also reflect survey effort.",
      source = "Carturan, B. & Peacock, S. (2026). Cleaned spawner survey data (v4). Pacific Salmon Foundation. https://doi.org/10.5281/zenodo.19959144. Derived from Fisheries and Oceans Canada NuSEDS.",
      licence = "CC BY 4.0 (cleaned data); Open Government Licence - Canada (NuSEDS)"
    ),
    by_year = by_year,
    streams_2018 = streams_2018 |> select(-streamid)
  ),
  here("data", "processed", "sockeye_fraser.json"),
  auto_unbox = TRUE, na = "null", digits = NA
)
message("File size: ", round(file.size(here("data", "processed", "sockeye_fraser.json")) / 1e3), " KB")
message("Saved: data/processed/sockeye_fraser.json")
