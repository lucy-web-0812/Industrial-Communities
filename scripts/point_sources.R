# Large point sources map....
library(tidyverse)
library(sf)
library(arrow)
library(leaflet)
library(plotly)




point_sources_raw <- open_dataset("processed_data/bz_point_sources")


uk_outline <- rnaturalearth::ne_countries(country = "United Kingdom", scale = "large", returnclass = "sf")


uk_outline_bng <- uk_outline |> 
  st_transform(crs = 27700) |> 
  select(geometry)


point_sources <- point_sources_raw |> 
  select(Easting, Northing, Year, Sector, PlantID, Emission, Site) |> 
  collect() |> 
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) |> 
  st_filter(uk_outline_bng)


write_rds(point_sources, "processed_data/point_sources.rds")


point_sources <- read_rds("processed_data/point_sources.rds")





# Diving deeper into those areas that have the largest sources.....

# Lets get the biggest point sources from 2023, find the coords and save to plot on a leaflet map!

point_sources |> 
  filter(Year == 2023) |> 
  arrange(desc(Emission)) |> 
  head(30) |> 
  ggplot() +
  geom_sf(data = uk_outline_bng) +
  geom_sf(aes(geometry = geometry, size = Emission)) 


# What % of the total emissions do the top x% contribute?


cumulative_values <- point_sources |>
  filter(Year == 2023) |>
  arrange(Emission) |>
  mutate(
    cumulative_emissions = cumsum(Emission),
    cumulative_pct = cumulative_emissions / sum(Emission),
    source_pct = row_number() / n()
  ) 


# The top 10 sources represent 55% of the total point source emissions... 

cumulative_values |> 
  arrange(desc(Emission)) |> 
  head(10) |> 
  summarise(total_emissions = sum(Emission)) # Total 439 tonnes compared to overall 9000 kg 


# And what is the total benzene emission compared to these top 10 sources...


cumulative_values |>
  ggplot(aes(source_pct, cumulative_pct)) +
  geom_line(linewidth = 1.2, colour = "#0072B2") +
  scale_x_continuous(
    labels = scales::percent,
    breaks = seq(0,1,.10),
    name = "Site (percentile sorted by Emissions)", 
    expand = c(0,0)
  ) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0,1,.10),
    limits = c(0,1),
    name = "Cumulative proportion of emissions", 
    expand = c(0,0)
  ) +
  geom_vline(xintercept = 0.95, linetype = 2, colour = "grey60") +
  geom_vline(xintercept = 0.9, linetype = 2, colour = "grey60") +
  geom_hline(yintercept = 0.1, linetype = 2, colour = "grey60") +
  geom_hline(yintercept = 0.06, linetype = 2, colour = "grey60") +
  theme_minimal(base_size = 16) 

ggsave("plots/basic_plots/point_sources_emission_profiles.png")






# Top 10 point sources to focus on... and also get the sources within 10km of these main sources... 
# So lets formally define our areas.... 


top_sources <- point_sources |> 
  filter(Year == 2023) |> 
  arrange(desc(Emission)) |> 
  head(10)


top_sources_locations <- top_sources |> 
  select(Site) 


# Keep what is within 10km

matches <- st_is_within_distance(
  point_sources,
  top_sources_locations,
  dist = 10000
)



# Fiddly bit getting the actual data from the df
data_for_industrial_communites <- tibble(
  source = rep(seq_along(matches), lengths(matches)),
  top_source = unlist(matches)
) |> 
  left_join(
    point_sources |> 
      st_drop_geometry() |> 
      mutate(source = row_number()),
    by = "source"
  ) |> 
  left_join(
    top_sources_locations |> 
      mutate(top_source = row_number()),
    by = "top_source",
    suffix = c("_source", "_top")
  ) |> 
  rename(source_Site = Site_source, top_Site = Site_top) |> 
  mutate(top_Site = ifelse(top_Site %in% c("South Killingholme Refinery", "Lindsey Oil Refinery"), "South Killingholme and Lindsey", top_Site)) 




# Okay two large sources close together... are there any differences in total emissions that capturing 
# only a tiny percentage difference...



ggplotly(
  data_for_industrial_communites |> 
  group_by(top_Site, Year) |> 
  summarise(total_benzene = sum(Emission, na.rm = T)) |> 
  ungroup() |> 
  ggplot() +
  geom_line(aes(x = Year, y = total_benzene, colour = top_Site), group = 1)
)




data_for_industrial_communites |>
  st_as_sf(sf_column_name = "geometry", crs = 27700) |> 
  ggplot() +
  geom_sf(data = uk_outline) +
  geom_sf(aes(geometry = geometry, colour = top_Site)) 


ggsave("plots/basic_plots/industrial_commmunities_locations.png")



data_for_industrial_communites_locations <- data_for_industrial_communites |> 
  group_by(top_Site) |> 
  distinct(source_Site, geometry) |> 
  as_tibble()

write_csv(data_for_industrial_communites_locations, "data/data_for_industrial_communities_locations.csv")

# Emissions at each site....
ggplotly(
data_for_industrial_communites |> 
  group_by(Year, top_Site) |> 
  summarise(total_emissions_per_site = sum(Emission, na.rm = T)) |> 
  st_drop_geometry() |> 
  ggplot() +
  geom_line(aes(x = Year, y = total_emissions_per_site, colour = top_Site)) +
  scale_y_continuous(name = "Emissions (tonnes)", breaks = seq(0,800,200), limits = c(0,800)) +
  scale_x_continuous(name = "Year") +
  theme_minimal(12) +
  theme(legend.position = "top")
)

ggsave("plots/basic_plots/industrial_commmunities_emissions_timeline.png")


data_for_industrial_communites |> 
  filter(top_Site == "Stanlow Manufacturing Complex") |> 
  distinct() |> 
  filter(Year == 2023)


point_sources |> 
  filter(Year == 2023) |> 
  summarise(total_emission = sum(Emission))
 


# Lets just look at Wilton 



data_for_industrial_communites |> 
  filter(top_Site == "Wilton") |> 
  ggplot(aes(x = Year, y = Emission, colour = factor(source_Site))) +
  geom_point() +
  geom_line()



# Looking at where the biggest point sources have been over the years....
top_sources_yearly <- point_sources |> 
  group_by(Year, PlantID) |> 
  summarise(Emission = sum(Emission, na.rm = T)) |> 
  arrange(desc(Emission)) |> 
  slice_head(n = 5) |> 
  mutate(ranking = row_number())


plotly::ggplotly(top_sources_yearly |> 
  ggplot(aes(x= Year, y = ranking, colour = PlantID)) +
  geom_line() +
  geom_point() +
  scale_y_reverse(breaks = seq(1,5,1))
)





# For all the top source locations in 2023, lets look at the deprivation in the surrounding areas.... 



coords <- st_coordinates(top_sources_locations)

top_sources_locations <- top_sources_locations |>
  mutate(
    Easting = coords[, 1],
    Northing = coords[, 2]
  )


profiles <- top_sources_locations |>
  mutate(
    profile = map2(
      Easting,
      Northing,
      population_profile_within_radius
    )
  ) |> 
  unnest()



profiles |> 
  group_by(IMD_decile) |> 
  summarise()
