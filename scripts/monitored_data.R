# Monitoring data.... 
library(tidyverse)
library(sf)
library(leaflet)
library(plotly)



###################################################
# NON_AUTOMATIC                                   #
###################################################


network_locations <- read_csv("data/measured_data/all_NAHN_locations.csv") |> 
  select(c(`Site Name`, Latitude, Longitude, `Environment Type`)) |> 
  rename(lng = Longitude, lat = Latitude) |> 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) |> 
  mutate(`Site Name` = 
           case_when(`Site Name` == "Birmingham Centre" ~ "Birmingham Roadside",
                     `Site Name` == "Camden Kerbside" ~ "Camden Kerbside(Swiss Cottage)",
                     `Site Name` == "Leeds Headingley Kerbside" ~ "Leeds Roadside",
                     .default    = `Site Name`
           )) |> 
  mutate(network_type = "non-automatic") |> 
  rename(station = `Site Name`, site_type = `Environment Type`)
  

monitoring_data_raw <- read_csv("data/measured_data/NAHN_measured_data_all.csv", col_names = F)

names(monitoring_data_raw) <- c("col1", "col2", "col3", "col4")

monitoring_data <- monitoring_data_raw |>
  mutate(
    year = str_extract(col1, "(?<=Data for year )\\d{4}")
  ) |>
  fill(year) 


# Site names occur immediately after "Multi-day data"
site_rows <- which(monitoring_data$col1 == "Multi-day data")

monitoring_data$site <- NA_character_
monitoring_data$site[site_rows + 1] <- monitoring_data$col1[site_rows + 1]

# Carry site names down until the next one
monitoring_data <- monitoring_data |>
  fill(site)

# Keep only rows beginning with a date
benzene_monitoring_data <- monitoring_data |>
  filter(str_detect(col1, "^\\d{2}/\\d{2}/\\d{4}$")) |>
  transmute(
    site,
    year = as.integer(year),
    start_date = dmy(col1),
    end_date = dmy(col2),
    benzene = as.numeric(col3),
    status_units = col4
  )




leaflet(network_locations) |> 
  addTiles() |> 
  addMarkers()
  



benzene_monitoring_data |> 
  filter(site == "Carlisle Morton A595") |> 
  ggplot(aes(x = end_date, y = benzene))+
  geom_point() +
  geom_line()



site_operational_periods <- benzene_monitoring_data |> 
  group_by(site) |> 
  filter(is.na(benzene) == F) |>
  group_by(site) |> 
  summarise(first_measurement = min(start_date), 
            latest_measurement = max(end_date))


ggplot(site_operational_periods) +
  geom_segment(aes(
    x = site,
    y = first_measurement,
    yend = latest_measurement,
    colour = ifelse(year(latest_measurement) == 2026, "red", "black")
  )) +
  scale_colour_identity() +
  coord_flip()



###################################################
# AUTOMATIC                                       #
###################################################

# Also looking at the automatic hydrocarbon network.... 


ahn_data_raw <- read_csv("data/measured_data/AHN_daily_mean.csv",
                         col_names = FALSE,
                         skip = 3, 
                         na = c("", "No data"),
                         show_col_types = FALSE)


meta_rows <- ahn_data_raw |> slice(1:7)


station_cols <- which(!is.na(meta_rows[1, ]) & meta_rows[1, ] != "")
station_cols <- station_cols[station_cols != 1]  # drop the (blank) Date/label column

station_meta <- tibble(
  station          = as.character(meta_rows[1, station_cols]),
  latitude         = as.numeric(meta_rows[2, station_cols]),
  longitude        = as.numeric(meta_rows[3, station_cols]),
  site_type        = as.character(meta_rows[4, station_cols]),
  zone             = as.character(meta_rows[5, station_cols]),
  agglomeration    = as.character(meta_rows[6, station_cols]),
  local_authority  = as.character(meta_rows[7, station_cols]),
  value_col        = as.numeric(station_cols)   # remember which raw column holds this station's data
)



data_block <- ahn_data_raw |>  slice(9:n())


end_row <- which(data_block[[1]] == "End")
if (length(end_row) > 0) data_block <- data_block |> slice(1:(end_row[1] - 1))

# Keep rows that actually have a date
data_block <- data_block |> filter(!is.na(.data[[names(data_block)[1]]]))

names(data_block)[1] <- "date_raw"

# For each station, grab its value column + the status column immediately
# after it, tag them with the station name, and stack everything long.
daily_long <- map_dfr(seq_len(nrow(station_meta)), function(i) {
  val_col <- station_meta$value_col[i]
  data_block |>
    transmute(
      date_raw,
      station = station_meta$station[i],
      value   = as.numeric(.data[[paste0("X", val_col)]]),
      status  = as.character(.data[[paste0("X", val_col + 1)]])
    )
})


ahn_tidy <- daily_long |>
  left_join(station_meta |> select(-value_col), by = "station") |>
  mutate(
    date = dmy(date_raw),
    status = substr(status,1,1)             # e.g. "V ugm-3" -> keep as-is, just trim
  ) |>
  select(
    date, station, value, status,
    latitude, longitude, site_type, zone, agglomeration, local_authority
  ) |>
  arrange(station, date) 


summary(ahn_tidy)

ggplotly(
ahn_tidy |> 
  filter(status == "V") |> 
  ggplot() +
  geom_line(aes(x = date, y = value, colour = station)) +
  facet_wrap(~station)
)


# And where is recording now? 

ahn_tidy |> 
  filter(year(date) %in% c(2024:2026)) |> 
  filter(is.na(value) == F) |> 
 # filter(status == "V") |> 
  ggplot() +
  geom_line(aes(x = date, y = value, colour = station, linetype = status)) +
  facet_wrap(~station, scales = "free")


# Only 4 Automatic sites.... 


# Combine the locations of the automatic and non-automatic locations 

ahn_locations <- ahn_tidy |> 
  filter(is.na(latitude) == F) |> 
  st_as_sf(coords = c("latitude", "longitude"), crs = 4326) |> 
  select(station, site_type, geometry) |> 
  unique() |> 
  mutate(network_type = "automatic")


###################################################
# COMPARISNG LOCATIONS                            #
###################################################


# --- Work out which sites have data in which years ---------------------------

non_automatic_site_years <- benzene_monitoring_data |> 
  filter(!is.na(benzene)) |> 
  mutate(year = year(start_date)) |>  
  distinct(site, year) |> 
  rename(station = site) |> 
  mutate(network_type = "non-automatic") |> 
  left_join(network_locations)

automatic_site_years <- ahn_tidy |> 
  filter(!is.na(value)) |> 
  mutate(year = year(date)) |> 
  distinct(station, year, site_type) |> 
  mutate(network_type = "automatic")

site_years_combined <- bind_rows(non_automatic_site_years, automatic_site_years)

# --- Count sites per year, per network -----------------------------------

sites_per_year <- site_years_combined |> 
  group_by(year, network_type, site_type) |> 
  summarise(n_sites = n_distinct(station), .groups = "drop")

# --- Timeline plot -----------------------------------------------------------

ggplot(sites_per_year, aes(x = year, y = n_sites, group = network_type, fill = site_type)) +
  geom_col(linewidth = 1, position = "stack") +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~network_type, scales = "free")
