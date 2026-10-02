# =============================================================================
# prepare_network_comparison.R
#
# Data for the "Comparing automatic and non-automatic measurements" section.
# Run AFTER prepare_monitoring_sites.R.
# =============================================================================

library(tidyverse)
library(sf)

ahn_site   <- "London Marylebone Road"
nahn_sites <- c("Camden Kerbside", "London Bloomsbury")

monitoring_sites   <- read_rds("processed_data/monitoring_sites.rds")
sites_active_years <- read_rds("processed_data/sites_active_years.rds")
ahn_hourly         <- read_rds("data/measured_data/ahn_hourly.rds")   # from openair::importUKAQ()
nahn_tubes         <- read_rds("data/measured_data/nahn_monitoring_data.rds")   |> 
  mutate(site = ifelse(site == "Camden Kerbside(Swiss Cottage)", "Camden Kerbside", site))

# ---- 1. The 10 closest automatic/non-automatic pairs that ran at the same time ----
ahn_locs  <- monitoring_sites |> filter(network == "AHN")  |> distinct(`Site Name`, .keep_all = TRUE)
nahn_locs <- monitoring_sites |> filter(network == "NAHN") |> distinct(`Site Name`, .keep_all = TRUE)

dist_km <- units::drop_units(st_distance(ahn_locs, nahn_locs)) / 1000

years_by_site <- sites_active_years |>
  st_drop_geometry() |>
  distinct(`Site Name`, network, year)

closest_pairs <- expand_grid(i = seq_len(nrow(ahn_locs)), j = seq_len(nrow(nahn_locs))) |>
  mutate(
    ahn_site    = ahn_locs$`Site Name`[i],
    ahn_type    = ahn_locs$`Environment Type`[i],
    nahn_site   = nahn_locs$`Site Name`[j],
    nahn_type   = nahn_locs$`Environment Type`[j],
    distance_km = dist_km[cbind(i, j)]
  ) |>
  select(-i, -j) |>
  left_join(
    years_by_site |>
      filter(network == "AHN") |>
      inner_join(years_by_site |> filter(network == "NAHN"),
                 by = "year", suffix = c("_ahn", "_nahn"),
                 relationship = "many-to-many") |>
      summarise(overlap_years = n_distinct(year),
                first_year = min(year), last_year = max(year),
                .by = c(`Site Name_ahn`, `Site Name_nahn`)),
    by = c("ahn_site" = "Site Name_ahn", "nahn_site" = "Site Name_nahn")
  ) |>
  filter(!is.na(overlap_years)) |>        # must have run at the same time
  slice_min(distance_km, n = 10, with_ties = FALSE)

# ---- 2. Locations of the three comparison sites ----
comparison_sites <- monitoring_sites |>
  filter((network == "AHN"  & `Site Name` == ahn_site) |
           (network == "NAHN" & `Site Name` %in% nahn_sites)) |>
  distinct(network, `Site Name`, .keep_all = TRUE) |>
  select(site = `Site Name`, network_label, `Environment Type`)

# ---- 3. Fortnightly measurements ----
# Tubes: verified, non-missing values only
tube_data <- nahn_tubes |>
  filter(site %in% nahn_sites) |>
  separate_wider_delim(status_units, delim = " ",
                       names = c("status", "units"), too_many = "merge") |>
  filter(status == "V", !is.na(benzene)) |>
  mutate(start_time = as.POSIXct(start_date, tz = "UTC"),
         end_time   = as.POSIXct(end_date,   tz = "UTC"))

# Automatic: average the hourly data over each tube period
tube_periods <- tube_data |>
  distinct(start_date, end_date, start_time, end_time) |>
  mutate(hours = as.numeric(difftime(end_time, start_time, units = "hours")))

ahn_fortnightly <- tube_periods |>
  inner_join(
    ahn_hourly |> filter(site == ahn_site) |> select(date, benzene),
    by = join_by(start_time <= date, end_time > date)
  ) |>
  summarise(
    capture = sum(!is.na(benzene)) / first(hours),
    benzene = mean(benzene, na.rm = TRUE),
    .by = c(start_date, end_date)
  ) |>
  filter(capture >= 0.75) |>                      # at least 75% of hours present
  mutate(site = ahn_site, method = "Automatic") |>
  select(-capture)

comparison_fortnightly <- bind_rows(
  ahn_fortnightly,
  tube_data |>
    transmute(site, start_date, end_date, benzene, method = "Non-automatic")
) |>
  # Keep only the period when the automatic data exist
  filter(start_date >= min(ahn_fortnightly$start_date),
         end_date   <= max(ahn_fortnightly$end_date)) |>
  arrange(site, start_date) |>
  mutate(
    label    = paste0(site, " (", tolower(method), ")"),
    mid_date = start_date + (end_date - start_date) / 2,
    # New line segment if more than 31 days since the previous period ended
    segment  = cumsum(is.na(lag(end_date)) | as.numeric(start_date - lag(end_date)) > 31),
    .by = site
  )

# ---- Save for the report ----
write_rds(closest_pairs,          "processed_data/closest_network_pairs.rds")
write_rds(comparison_sites,       "processed_data/comparison_sites.rds")
write_rds(comparison_fortnightly, "processed_data/comparison_fortnightly.rds")
