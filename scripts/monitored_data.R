# Monitoring data.... 


network_locations <- read_csv("data/measured_data/NAHN_locations.csv") |> 
  select(c(`Site Name`, Latitude, Longitude)) |> 
  rename(lng = Longitude, lat = Latitude) |> 
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

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
