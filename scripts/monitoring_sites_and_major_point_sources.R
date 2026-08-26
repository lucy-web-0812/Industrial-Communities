# Lets look at where we are monitoring data versus where we are polluting 

library(tidyverse)
library(sf)
library(leaflet)
library(ggspatial)
library(units)
library(nasapower)
library(openair)


all_NAHN_locations <- read_csv("data/measured_data/all_NAHN_locations.csv") |> 
  mutate(network = "NAHN")

all_AHN_locations <- read_csv("data/measured_data/all_AHN_locations.csv") |> 
  mutate(network = "AHN")

current_NAHN_locations <- read_csv("data/measured_data/current_NAHN_locations.csv") |> 
  mutate(network = "NAHN")

current_AHN_locations <- read_csv("data/measured_data/current_AHN_locations.csv") |> 
  mutate(network = "AHN")



current_sites_nahn <- current_NAHN_locations |> 
  select(`Site Name`) |> 
  pull()


all_NAHN_locations_with_status <- all_NAHN_locations |> 
  mutate(status = ifelse(`Site Name` %in% current_sites_nahn, "active", "closed"))





current_sites_ahn <- current_AHN_locations |> 
  select(`Site Name`) |> 
  pull()


all_AHN_locations_with_status <- all_AHN_locations |> 
  mutate(status = ifelse(`Site Name` %in% current_sites_ahn, "active", "closed"))



monitoring_sites <- rbind(all_NAHN_locations_with_status,
                          all_AHN_locations_with_status ) |> 
  st_as_sf(coords = c( "Longitude", "Latitude"), crs = 4326) |> 
  mutate(test_of_closed = ifelse(is.na(`End Date`) == T, "active", "closed"), 
         test = status == test_of_closed) 


monitoring_sites |> 
  st_drop_geometry() |> 
  group_by(network, status) |> 
  summarise(count = n())


start_and_ends_NAHN <- read_csv("data/measured_data/start_and_end_NAHN.csv") |> 
  mutate(network = "NAHN")
start_and_ends_AHN <- read_csv("data/measured_data/start_and_end_AHN.csv") |> 
  mutate(network = "AHN")

sites_active_years <- start_and_ends_NAHN |>
  rbind(start_and_ends_AHN) |> 
  mutate(
    Start = dmy(Start),
    End = dmy(End)
  ) |>
  rowwise() |>
  mutate(
    year = list(seq(
      year(Start),
      year(coalesce(End, Sys.Date()))
    ))
  ) |>
  ungroup() |>
  unnest(year) |> 
  select(`Site Name`, network, year) |> 
  left_join(monitoring_sites |> select('Site Name', geometry)) |> 
  st_as_sf(crs = 4326)




sites_per_year <- sites_active_years |>
  count(year, network, name = "n_sites")



ggplot(sites_per_year) +
  geom_line(aes(x = year, y = n_sites, colour = network)) +
  geom_point(aes(x = year, y = n_sites, colour = network)) +
  scale_colour_manual(values = c("#E78AC3", "#A6D854")) +
  scale_y_continuous(name = "Number of sites", expand = c(0,0), limits = c(0,NA)) +
  scale_x_continuous(name = "Year") +
  theme_minimal(16) +
  theme(axis.line = element_line())



ggsave("plots/basic_plots/ahn_vs_nahn_numbers.png")


ggplot(monitoring_sites) +
  annotation_map_tile(zoom = 7, type = "cartolight") +
  geom_sf(aes(geometry = geometry, colour = status, shape = network), size = 2) +
  scale_colour_manual(values = c("#E78AC3", "#A6D854")) 




# And lets see if we can also add in our major point sources! 

point_sources <- read_rds("processed_data/point_sources.rds")

major_sources_per_year <- point_sources |> 
  group_by(Year) |> 
  slice_max(order_by = Emission, n = 100, with_ties = FALSE) |> 
  st_transform(4326)


major_sources_2023 <- point_sources |> 
  filter(Year == "2023") |> 
  arrange(desc(Emission)) |> 
  head(20)|> 
  st_transform(4326)



ggplot(major_sources_per_year) +
  geom_sf(aes(geometry = geometry, size = Emission)) +
  facet_wrap(~Year, ncol = 10)



point_sources |> 
  group_by(Year) |> 
  summarise(total_emissions_per_year = sum(Emission, na.rm = T)) |> 
  ggplot() +
  geom_point(aes(x = Year, y = total_emissions_per_year)) +
  geom_line(aes(x = Year, y = total_emissions_per_year)) +
  scale_y_continuous(limits = c(0,NA)) 

  

ggplot(monitoring_sites) +
  annotation_map_tile(zoom = 7, type = "cartolight") +
  geom_sf(aes(geometry = geometry, colour = status, shape = network), size = 2) +
  scale_colour_manual(values = c("darkgreen","red")) +
  ggnewscale::new_scale_colour() +
  geom_sf(data = major_sources_2023, aes(geometry = geometry, colour = "Major point source"), shape = 4) +
  scale_colour_manual(
    name = NULL,
    values = c("Major point source" = "black")
  ) +
  facet_grid(rows = vars(network), cols = vars(status)) +
  theme_minimal(16) +
  theme(legend.position = "top", 
        axis.text = element_blank())



ggsave("plots/basic_plots/station_statuses.png")


# And turn into a leaflet plot..... 


leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  
  addCircleMarkers(
    data = monitoring_sites |> filter(status == "active"),
    radius = 5,
    color = "darkgreen",
    fillOpacity = 0.8,
    stroke = FALSE,
    group = "Active monitoring sites"
  ) |>
  
  addCircleMarkers(
    data = monitoring_sites |> filter(status != "active"),
    radius = 5,
    color = "red",
    fillOpacity = 0.8,
    stroke = FALSE,
    group = "Inactive monitoring sites"
  ) |>
  addCircleMarkers(
    data = major_sources_2023,
    radius = 7,
    color = "black",
    fill = FALSE,
    weight = 2,
    group = "Major point sources"
  ) |>
  
  addLayersControl(
    overlayGroups = c(
      "Active monitoring sites",
      "Inactive monitoring sites",
      "Major point sources"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )




# This is absolute rubbish as need to look at what is available in terms of sites each year!!! 


closest_measuring_source <- major_sources_per_year |> 
  group_by(Year) |>
  group_modify(~ {
    
    year_sites <- sites_active_years |>
      filter(year == .y$Year)
    
    .x |>
      mutate(
        nearest_idx = st_nearest_feature(geometry, year_sites),
        nearest_site = year_sites$`Site Name`[nearest_idx],
        distance_m = st_distance(
          geometry,
          year_sites$geometry[nearest_idx],
          by_element = TRUE
        )
      )
  }) |>
  ungroup()


# Check if we are being mislead by new sources post-2020.....
new_sources <- read_csv("data/processed_data/new_naei_post_2020_sources.csv")







stats <- closest_measuring_source |> 
  st_drop_geometry() |> 
  group_by(Year) |> 
  summarise(
    mean_distance = mean(distance_m, na.rm = TRUE),
    median_distance = median(distance_m, na.rm = TRUE),
    weighted_distance = weighted.mean(
      distance_m,
      w = Emission,
      na.rm = TRUE
    ),
    p90_distance = quantile(
      distance_m,
      0.9,
      na.rm = TRUE
    )
  )


stats |> 
  units::drop_units() |> 
  ggplot() +
  geom_line(aes(x = Year, y = weighted_distance, linetype = "Weighted Mean")) +
  geom_line(aes(x = Year, y = median_distance, linetype = "Median")) +
  scale_y_continuous(limits = c(0,NA)) +
  theme_minimal(16) +
  theme(axis.line = element_line())




closest_measuring_source |> 
  ggplot() +
  geom_boxplot(aes(x = Year, y = (distance_m), group = Year))




closest_measuring_source_current <-  major_sources_2023 |> 
  mutate(
    nearest_idx = st_nearest_feature(geometry, current_monitoring_sites),
    nearest_site = current_monitoring_sites$`Site Name`[nearest_idx],
    distance_m = st_distance(
      geometry,
      current_monitoring_sites$geometry[nearest_idx],
      by_element = TRUE
    )
  )


stats_current <- closest_measuring_source_current |> 
  summarise(mean_distance = mean(distance_m), 
            median_distance = median(distance_m),
            min_distance = min(distance_m), 
            max_distance = max(distance_m)) |> 
  st_drop_geometry()




# I think will need to look at all of the measured data to be able to see when the sites where active...
# When was the peak? 
# What are we looking at today?


