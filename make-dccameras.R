# make-dccameras.R -------------------------------------------------------------
#
# Builds data/dccameras.csv: one row per fixed automated traffic camera in
# Washington, DC, with the number of tickets it issued over twelve months and
# the characteristics of the road and neighborhood it sits in. Also builds
# data/dctracts.csv (+ .gpkg, with polygons): one row per 2020 census tract,
# with the number of cameras placed in it. And data/dccameras-monthly.csv: every
# live camera-month since September 2022. And data/dcwater.gpkg: rivers and
# lakes, for the maps.
#
# Window: September 2025 through August 2026, the last twelve complete months
# DDOT had published when this was built (2026-10-07). A camera's tickets are
# summed over the months in that window when its status was "Live"; warning,
# test, idle, and configuration months are dropped, as on the
# famodeo.github.io/dc-traffic-cameras site that prompted this.
#
# Sources, all public:
#   DDOT Automated Safety Cameras, layers 43 (locations) and 46 (violations by
#     month), Open Data DC, CC BY 4.0
#   DDOT Traffic Volume 2024 (AADT by road segment), Open Data DC
#   DDOT Roadway Functional Classification (lanes, functional class), Open Data DC
#   MPD/DDOT Vehicular Crash Data, 2019-2022, Open Data DC
#   Census tracts 2020 (polygons), Open Data DC
#   ACS 2020-2024 5-year estimates, tract level, Census API (needs
#     CENSUS_API_KEY in ~/.Renviron)
#
# Raw pulls are cached as .rds in data/dccameras-raw/ so the build reruns
# offline; delete that folder to re-fetch.
#
# Packages: tidyverse, sf, jsonlite, here -- all already in the project library.

library(tidyverse)
library(sf)
library(jsonlite)
library(here)

raw_dir <- here("data", "dccameras-raw")
dir.create(raw_dir, showWarnings = FALSE)

gis <- "https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/"

# ArcGIS REST helpers -----------------------------------------------------------
# The server returns at most 1,000 records per request, so both helpers page with
# resultOffset until a short page comes back.

# the DC server drops the occasional connection; retry before giving up
retry <- function(f, url, tries = 5) {
  for (k in seq_len(tries)) {
    res <- tryCatch(f(url), error = function(e) e)
    if (!inherits(res, "error")) return(res)
    Sys.sleep(2 * k)
  }
  stop(conditionMessage(res))
}

arc_url <- function(layer, q) {
  paste0(gis, layer, "/query?",
         paste(names(q), map_chr(q, ~ URLencode(as.character(.x), reserved = TRUE)),
               sep = "=", collapse = "&"))
}

arc_table <- function(layer, where = "1=1", fields = "*") {
  out <- list(); off <- 0
  repeat {
    q <- list(where = where, outFields = fields, returnGeometry = "false",
              orderByFields = "OBJECTID", resultOffset = off,
              resultRecordCount = 1000, f = "json")
    url <- arc_url(layer, q)
    page <- retry(fromJSON, url)$features$attributes
    if (is.null(page) || nrow(page) == 0) break
    out[[length(out) + 1]] <- page
    off <- off + nrow(page)
    if (nrow(page) < 1000) break
  }
  bind_rows(out) %>% as_tibble()
}

arc_sf <- function(layer, where = "1=1", fields = "*") {
  out <- list(); off <- 0
  repeat {
    q <- list(where = where, outFields = fields, outSR = "4326",
              orderByFields = "OBJECTID", resultOffset = off,
              resultRecordCount = 1000, f = "geojson")
    url <- arc_url(layer, q)
    page <- retry(function(u) st_read(u, quiet = TRUE), url)
    if (nrow(page) == 0) break
    out[[length(out) + 1]] <- page
    off <- off + nrow(page)
    if (nrow(page) < 1000) break
  }
  bind_rows(out)
}

# fetch once, then read the cached copy
cached <- function(name, expr) {
  f <- file.path(raw_dir, paste0(name, ".rds"))
  if (!file.exists(f)) saveRDS(expr, f)
  readRDS(f)
}

# Pull --------------------------------------------------------------------------

cams_raw <- cached("cameras", arc_table("Public_Safety_WebMercator/MapServer/43"))

counts_raw <- cached("counts", arc_table(
  "Public_Safety_WebMercator/MapServer/46",
  fields = "ENFORCEMENT_SPACE_CODE,YEAR_MONTH,CAMERA_STATUS,NUM_VIOLATIONS,ENFORCEMENT_TYPE"))

aadt_raw <- cached("aadt2024", arc_sf(
  "Transportation_TrafficVolume_WebMercator/MapServer/5", fields = "ROUTEID,AADT"))

roads_raw <- cached("roads", arc_sf(
  "Transportation_WebMercator/MapServer/48",
  fields = "ROUTENAME,TOTALTRAVELLANES,DCFUNCTIONALCLASS"))

crashes_raw <- cached("crashes2019_2022", arc_table(
  "Public_Safety_WebMercator/MapServer/24",
  where = "REPORTDATE >= DATE '2019-01-01' AND REPORTDATE < DATE '2023-01-01'",
  fields = "LATITUDE,LONGITUDE,OBJECTID"))

tracts_raw <- cached("tracts2020", arc_sf(
  "Demographic_WebMercator/MapServer/8", fields = "GEOID,ALAND"))

water_raw <- cached("water", arc_sf(
  "Environment_Water_WebMercator/MapServer/24",
  where = "DESCRIPTION IN ('River', 'Lake')", fields = "DESCRIPTION"))

# ACS 2020-2024 5-year, DC tracts. Variables:
#   B01003_001  total population
#   B02001_001  race: total;  B02001_003  Black alone
#   B19013_001  median household income
#   B08201_001  households;   B08201_002  households with no vehicle
acs_raw <- cached("acs2024", {
  vars <- c("B01003_001E", "B02001_001E", "B02001_003E", "B19013_001E",
            "B08201_001E", "B08201_002E")
  m <- fromJSON(paste0("https://api.census.gov/data/2024/acs/acs5?get=",
                       paste(vars, collapse = ","),
                       "&for=tract:*&in=state:11&key=", Sys.getenv("CENSUS_API_KEY")))
  as_tibble(m[-1, ], .name_repair = ~ m[1, ])
})

# Tickets per camera over the window --------------------------------------------

fixed_types <- c("Speed", "Red Light", "Stop Sign")

tickets <- counts_raw %>%
  mutate(month = as.Date(as.POSIXct(YEAR_MONTH / 1000, origin = "1970-01-01",
                                    tz = "America/New_York"))) %>%
  filter(ENFORCEMENT_TYPE %in% fixed_types,
         CAMERA_STATUS == "Live",
         month >= as.Date("2025-09-01"), month < as.Date("2026-09-01")) %>%
  group_by(id = ENFORCEMENT_SPACE_CODE) %>%
  summarise(tickets = sum(NUM_VIOLATIONS), months = n(), .groups = "drop")

# The full monthly panel: every live camera-month of a fixed camera, Sept 2022
# on. Not used in the cross-section; kept for the novelty-effect check and for
# a panel version.

monthly <- counts_raw %>%
  mutate(month = as.Date(as.POSIXct(YEAR_MONTH / 1000, origin = "1970-01-01",
                                    tz = "America/New_York"))) %>%
  filter(ENFORCEMENT_TYPE %in% fixed_types, CAMERA_STATUS == "Live") %>%
  transmute(id = ENFORCEMENT_SPACE_CODE, month, type = ENFORCEMENT_TYPE,
            tickets = NUM_VIOLATIONS) %>%
  arrange(id, month)

# Cameras, as points in a projected CRS (Maryland State Plane, meters) ----------

cams <- cams_raw %>%
  filter(ENFORCEMENT_TYPE %in% fixed_types) %>%
  transmute(id = ENFORCEMENT_SPACE_CODE,
            type = recode(ENFORCEMENT_TYPE, "Speed" = "speed",
                          "Red Light" = "red light", "Stop Sign" = "stop sign"),
            location = LOCATION_DESCRIPTION,
            speedlimit = SPEED_LIMIT,
            ward = WARD,
            lat = CAMERA_LATITUDE, lon = CAMERA_LONGITUDE) %>%
  inner_join(tickets, by = "id") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(26985)

# Traffic volume: AADT of the nearest 2024 count segment ------------------------
# A camera at an intersection may sit nearer the cross street than the street it
# watches; dist_aadt is kept so that mismatch can be checked.

aadt <- aadt_raw %>% filter(!st_is_empty(geometry), AADT > 0) %>% st_transform(26985)
i <- st_nearest_feature(cams, aadt)
cams$aadt <- aadt$AADT[i]
cams$dist_aadt <- as.numeric(st_distance(cams, aadt[i, ], by_element = TRUE))

# Roads: lanes and functional class of the nearest roadway block ----------------

roads <- roads_raw %>% filter(!st_is_empty(geometry)) %>% st_transform(26985)
j <- st_nearest_feature(cams, roads)
cams$lanes <- roads$TOTALTRAVELLANES[j]
cams$roadclass <- recode(roads$DCFUNCTIONALCLASS[j],
                         `11` = "freeway", `12` = "freeway",
                         `14` = "principal arterial", `16` = "minor arterial",
                         `17` = "collector", `18` = "collector", `19` = "local")

# Crashes within 150 m, 2019-2022 -----------------------------------------------
# Four years ending before the 2023-24 installation wave, so the count measures
# the danger at the site rather than the camera's effect on it.

crashes <- crashes_raw %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE), LATITUDE > 38, LONGITUDE < -76) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(26985)
cams$crashes <- lengths(st_is_within_distance(cams, crashes, dist = 150))

# Tract: nearest tract polygon, then ACS -----------------------------------------
# Nearest rather than within: twelve cameras on Southern Ave and Eastern Ave sit
# on the DC-Maryland line, 2-10 m outside every DC tract polygon. For every other
# camera the nearest tract is the one containing it.

acs <- acs_raw %>%
  mutate(GEOID = paste0(state, county, tract),
         across(ends_with("E"), as.numeric)) %>%
  transmute(GEOID,
            tractpop = B01003_001E,
            pctblack = 100 * B02001_003E / B02001_001E,
            medinc = if_else(B19013_001E < 0, NA_real_, B19013_001E / 1000),
            pctnocar = 100 * B08201_002E / B08201_001E)

tracts <- tracts_raw %>% st_transform(26985) %>% select(GEOID, ALAND)

dccameras <- cams %>%
  st_join(tracts, join = st_nearest_feature) %>%
  left_join(acs, by = "GEOID") %>%
  mutate(popdens = tractpop / (ALAND / 1e6) / 1000) %>%   # thousands per km^2
  st_drop_geometry() %>%
  select(id, type, location, ward, tract = GEOID, lat, lon,
         tickets, months, speedlimit, aadt, dist_aadt, lanes,
         roadclass, crashes, pctblack, medinc, pctnocar, popdens) %>%
  arrange(id)

# Tract file: placement --------------------------------------------------------
# One row per 2020 tract. majorkm is the length of every road above the local
# class -- freeways, arterials, and collectors -- which is where all but the
# local-street cameras sit, i.e. the road network a camera could be placed on. Tickets are annualized (tickets / months * 12) so
# cameras live part of the window count at a full-year rate.

major <- roads %>%
  filter(DCFUNCTIONALCLASS != 19) %>%
  st_intersection(tracts %>% select(GEOID)) %>%
  mutate(km = as.numeric(st_length(geometry)) / 1000) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(majorkm = sum(km), .groups = "drop")

dctracts <- tracts %>%
  mutate(areakm2 = ALAND / 1e6,
         crashes = lengths(st_intersects(geometry, crashes))) %>%
  left_join(acs, by = "GEOID") %>%
  left_join(major, by = "GEOID") %>%
  left_join(dccameras %>%
              group_by(GEOID = tract) %>%
              summarise(ncams = n(), tickets = sum(tickets / months * 12),
                        .groups = "drop"),
            by = "GEOID") %>%
  mutate(across(c(majorkm, ncams, tickets), ~ replace_na(.x, 0)),
         popdens = tractpop / areakm2 / 1000) %>%
  select(tract = GEOID, ncams, tickets, majorkm, areakm2, crashes,
         tractpop, pctblack, medinc, pctnocar, popdens)

# Checks --------------------------------------------------------------------------

stopifnot(
  !anyDuplicated(dccameras$id),
  all(dccameras$months >= 1 & dccameras$months <= 12),
  all(dccameras$tickets > 0),
  all(!is.na(dccameras$tract)),
  !anyDuplicated(dctracts$tract),
  sum(dctracts$ncams) == nrow(dccameras),
  !anyDuplicated(monthly[c("id", "month")])
)

write_csv(dccameras, here("data", "dccameras.csv"))
write_csv(st_drop_geometry(dctracts), here("data", "dctracts.csv"))
write_csv(monthly, here("data", "dccameras-monthly.csv"))
st_write(st_transform(dctracts, 4326), here("data", "dctracts.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)
# water: rivers and lakes dissolved by type and simplified to 5 m, which is
# plenty for a city-scale map and keeps the rendered page small
dcwater <- water_raw %>%
  st_make_valid() %>%
  st_transform(26985) %>%
  group_by(type = DESCRIPTION) %>%
  summarise(.groups = "drop") %>%
  st_simplify(dTolerance = 5) %>%
  st_transform(4326)
st_write(dcwater, here("data", "dcwater.gpkg"), delete_dsn = TRUE, quiet = TRUE)

message(nrow(dccameras), " cameras written to data/dccameras.csv; ",
        nrow(dctracts), " tracts to data/dctracts.csv and .gpkg; water to dcwater.gpkg")
