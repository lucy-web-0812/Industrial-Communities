# =============================================================================
# prepare_monitoring_sites.R
#
# Data preparation for the Monitoring and "Location of Emissions relative to
# Monitoring" sections of benzene_report.qmd.
#
# Run AFTER prepare_point_sources.R (it uses point_sources.rds and top_sites.rds).
# =============================================================================

library(tidyverse)
library(sf)

read_sites <- function(path, network) {
  read_csv(path, show_col_types = FALSE) |>
    mutate(network = network)
}

# ---- Monitoring site locations, with active/closed status -------------------
current_names <- bind_rows(
  read_sites("data/measured_data/current_NAHN_locations.csv", "NAHN"),
  read_sites("data/measured_data/current_AHN_locations.csv",  "AHN")
) |>
  distinct(network, `Site Name`) |>
  mutate(status = "active")

monitoring_sites <- bind_rows(
  read_sites("data/measured_data/all_NAHN_locations.csv", "NAHN"),
  read_sites("data/measured_data/all_AHN_locations.csv",  "AHN")
) |>
  # Match on network AND name: some sites are in both networks
  left_join(current_names, by = c("network", "Site Name")) |>
  mutate(
    status        = coalesce(status, "closed"),
    network_label = if_else(network == "AHN", "Automatic", "Non-automatic"),
    # One site (2008–2010) was classified "Unknown Industrial"
    `Environment Type` = if_else(`Environment Type` == "Unknown Industrial",
                                 "Urban Industrial", `Environment Type`)
  ) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  st_transform(27700)                          # metres, same CRS as point sources

# ---- Which sites were running in each year ----------------------------------
# A site counts as active in a year if it ran for any part of that year
sites_active_years <- bind_rows(
  read_sites("data/measured_data/start_and_end_NAHN.csv", "NAHN"),
  read_sites("data/measured_data/start_and_end_AHN.csv",  "AHN")
) |>
  mutate(
    Start = dmy(Start),
    End   = dmy(End),
    year  = map2(year(Start), year(coalesce(End, Sys.Date())), seq)
  ) |>
  unnest(year) |>
  distinct(`Site Name`, network, year) |>
  left_join(
    monitoring_sites |> select(`Site Name`, network, network_label, `Environment Type`),
    by = c("Site Name", "network")
  ) |>
  st_as_sf() |>
  filter(!st_is_empty(geometry))               # drop sites with no location

# ---- Major point sources each year (top 100 SITES, not rows) ----------------
point_sources <- read_rds("processed_data/point_sources.rds")

site_locations <- point_sources |>
  group_by(Site) |>
  slice(1) |>
  ungroup() |>
  select(Site)

major_sources_by_year <- point_sources |>
  st_drop_geometry() |>
  summarise(Emission = sum(Emission, na.rm = TRUE), .by = c(Year, Site)) |>
  slice_max(Emission, n = 100, by = Year, with_ties = FALSE) |>
  left_join(site_locations, by = "Site") |>
  st_as_sf()

# ---- Distance from each major source to the nearest monitor RUNNING that year
nearest_monitor_by_year <- major_sources_by_year |>
  group_split(Year) |>
  map(\(yr) {
    monitors <- sites_active_years |> filter(year == yr$Year[1])
    if (nrow(monitors) == 0) return(NULL)
    idx <- st_nearest_feature(yr, monitors)
    yr |>
      mutate(
        nearest_site    = monitors$`Site Name`[idx],
        nearest_network = monitors$network_label[idx],
        distance_km     = as.numeric(st_distance(geometry, monitors$geometry[idx],
                                                 by_element = TRUE)) / 1000
      )
  }) |>
  bind_rows() |>
  st_drop_geometry()

# ---- Today: nearest ACTIVE monitor to each of the top 10 sites --------------
top_sites       <- read_rds("processed_data/top_sites.rds")
active_monitors <- monitoring_sites |> filter(status == "active")

idx <- st_nearest_feature(top_sites, active_monitors)

nearest_monitor_current <- top_sites |>
  mutate(
    nearest_site    = active_monitors$`Site Name`[idx],
    nearest_network = active_monitors$network_label[idx],
    nearest_type    = active_monitors$`Environment Type`[idx],
    distance_km     = as.numeric(st_distance(geometry, active_monitors$geometry[idx],
                                             by_element = TRUE)) / 1000,
    monitors_within_10km = lengths(st_is_within_distance(geometry, active_monitors,
                                                         dist = 10000))
  ) |>
  st_drop_geometry() |>
  select(community, Site, Emission, nearest_site, nearest_network,
         nearest_type, distance_km, monitors_within_10km) |>
  arrange(desc(Emission))

# ---- Save for the report ----------------------------------------------------
write_rds(monitoring_sites,        "processed_data/monitoring_sites.rds")
write_rds(sites_active_years,      "processed_data/sites_active_years.rds")
write_rds(nearest_monitor_by_year, "processed_data/nearest_monitor_by_year.rds")
write_rds(nearest_monitor_current, "processed_data/nearest_monitor_current.rds")
