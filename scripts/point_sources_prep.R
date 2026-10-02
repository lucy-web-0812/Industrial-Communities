# =============================================================================
# prepare_point_sources.R
#
# Slow data preparation for the point-source section of benzene_report.qmd.
# Run this ONCE (or whenever the NAEI point-source data changes). The report
# then just reads the .rds files it writes, so rendering stays fast.
# =============================================================================

library(tidyverse)
library(sf)
library(arrow)

# ---- UK outline (British National Grid) -------------------------------------
uk_outline_bng <- rnaturalearth::ne_countries(
  country = "United Kingdom", scale = "large", returnclass = "sf"
) |>
  st_transform(crs = 27700) |>
  select(geometry)

# ---- Point sources ----------------------------------------------------------
# NOTE: check the units of `Emission` in the NAEI point-source data (tonnes vs kg)
# and keep them consistent with the national totals used in the report.
point_sources <- open_dataset("processed_data/bz_point_sources") |>
  select(Easting, Northing, Year, Sector, PlantID, Emission, Site) |>
  collect() |>
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700, remove = FALSE) |>
  st_filter(uk_outline_bng) |>
  mutate(source_id = row_number())          # stable ID, used in the joins below

# ---- Define "industrial communities" ----------------------------------------
# The 20 largest sites in 2023, and every point source within 10 km of them.

# Sum per site first, so one site with several rows can't take two top-10 places
top_sites <- point_sources |>
  filter(Year == 2023) |>
  group_by(Site) |>
  summarise(Emission = sum(Emission, na.rm = TRUE)) |>   # sf keeps one point per site
  slice_max(Emission, n = 20) |>
  mutate(
    top_id = row_number(),
    # Two refineries next to each other: treat as one community
    community = if_else(
      Site %in% c("South Killingholme Refinery", "Lindsey Oil Refinery"),
      "South Killingholme and Lindsey",
      Site
    )
  )

matches <- st_is_within_distance(point_sources, top_sites, dist = 10000)

industrial_communities <- tibble(
  source_id = rep(seq_along(matches), lengths(matches)),
  top_id    = unlist(matches)
) |>
  left_join(st_drop_geometry(top_sites) |> select(top_id, community), by = "top_id") |>
  # A source within 10 km of BOTH refineries would otherwise be counted twice
  distinct(source_id, community) |>
  # Keep the SOURCE's own location (not the top site's)
  left_join(point_sources, by = "source_id") |>
  st_as_sf(sf_column_name = "geometry", crs = 27700)

# ---- Save for the report ----------------------------------------------------
write_rds(uk_outline_bng,         "processed_data/uk_outline_bng.rds")
write_rds(point_sources,          "processed_data/point_sources.rds")
write_rds(top_sites,              "processed_data/top_sites.rds")
write_rds(industrial_communities, "processed_data/industrial_communities.rds")

# Locations table (source sites in each community), if still needed elsewhere
industrial_communities |>
  distinct(community, Site, Easting, Northing) |>
  st_drop_geometry() |>
  write_csv("data/data_for_industrial_communities_locations.csv")