# 02c_smoke_plumes.R
# Air chapter: satellite smoke plumes over the Fraser basin, July to September 2018.
#
# NOAA analysts trace smoke from satellite images every day and publish the
# outlines as shapefiles, each plume marked Light, Medium, or Heavy.
# Source: NOAA/NESDIS Hazard Mapping System (HMS) Fire and Smoke Product
# Licence: public domain (US federal government data)
#
# Downloads one small zip per day (92 days) into data/raw/hms_2018/, keeps only
# the part over southern and central BC, simplifies the outlines so the file is
# small enough for phones, and saves one GeoJSON file.
#
# Run from the two-rivers RStudio project.
# Output: data/processed/smoke_plumes_2018.geojson

library(sf)
library(dplyr)
library(here)

options(timeout = 600)
sf_use_s2(FALSE)

dates   <- seq(as.Date("2018-07-01"), as.Date("2018-09-30"), by = "day")
raw_dir <- here("data", "raw", "hms_2018")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
base_url <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile"

# Box around the Fraser basin and the Lower Mainland (longitude, latitude)
box <- st_bbox(c(xmin = -128, ymin = 48.8, xmax = -116, ymax = 55.5), crs = st_crs(4326))

# Older files store density as a number (5, 16, 27); newer ones as words
density_label <- function(x) {
  num <- suppressWarnings(as.numeric(as.character(x)))
  word <- tolower(trimws(as.character(x)))
  case_when(
    num == 5  | word == "light"  ~ "Light",
    num == 16 | word == "medium" ~ "Medium",
    num == 27 | word == "heavy"  ~ "Heavy",
    TRUE ~ "Unknown"
  )
}

read_day <- function(d) {
  ymd <- format(d, "%Y%m%d")
  zip <- file.path(raw_dir, paste0("hms_smoke", ymd, ".zip"))
  if (!file.exists(zip)) {
    url <- sprintf("%s/%s/%s/hms_smoke%s.zip", base_url, format(d, "%Y"), format(d, "%m"), ymd)
    ok <- tryCatch({ download.file(url, zip, mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) FALSE)
    if (!ok) { unlink(zip); message("No file for ", d); return(NULL) }
  }
  exdir <- file.path(raw_dir, ymd)
  unzip(zip, exdir = exdir)
  shp <- list.files(exdir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)[1]
  x <- st_read(shp, quiet = TRUE)
  if (is.na(st_crs(x))) st_crs(x) <- 4326 else x <- st_transform(x, 4326)
  dens_col <- intersect(c("Density", "DENSITY", "density"), names(x))[1]
  x <- x |>
    st_make_valid() |>
    mutate(date = d,
           density = if (is.na(dens_col)) "Unknown" else density_label(.data[[dens_col]])) |>
    select(date, density)
  x <- suppressWarnings(st_crop(x, box))
  x[!st_is_empty(x), ]
}

# ---- 1. Download and read every day ----
days <- lapply(dates, read_day)
got  <- sum(!vapply(days, is.null, logical(1)))
plumes <- do.call(rbind, days[!vapply(days, is.null, logical(1))])

# ---- 2. Simplify and merge: one shape per day per density ----
plumes_small <- plumes |>
  st_transform(3005) |>                      # BC Albers, in metres
  st_simplify(dTolerance = 2000, preserveTopology = TRUE) |>
  st_make_valid() |>
  group_by(date, density) |>
  summarise(.groups = "drop") |>
  st_transform(4326) |>
  mutate(date = format(date, "%Y-%m-%d"),
         density = factor(density, levels = c("Light", "Medium", "Heavy", "Unknown"))) |>
  arrange(date, density)

# ---- 3. Save ----
out <- here("data", "processed", "smoke_plumes_2018.geojson")
if (file.exists(out)) file.remove(out)
st_write(plumes_small, out, driver = "GeoJSON", quiet = TRUE,
         layer_options = c("COORDINATE_PRECISION=3", "RFC7946=YES"))

# ---- 4. Checks ----
message("\n--- Checks ---")
message("Days requested: ", length(dates), "; days with a file: ", got)
message("Days with any smoke over the box: ", length(unique(plumes_small$date)))
message("Day-and-density shapes by density:")
print(table(plumes_small$density))
heavy_days <- sort(unique(plumes_small$date[plumes_small$density == "Heavy"]))
message("Days with heavy smoke: ", length(heavy_days))
message("First and last heavy day: ", paste(range(heavy_days), collapse = " to "))
message("File size: ", round(file.size(out) / 1e3), " KB")
message("Saved: data/processed/smoke_plumes_2018.geojson")
