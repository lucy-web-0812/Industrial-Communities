# Large point sources map....
library(tidyverse)
library(sf)
library(arrow)
library(dbscan)
library(leaflet)




point_sources_raw <- open_dataset("processed_data/bz_point_sources")


uk_outline <- rnaturalearth::ne_countries(country = "United Kingdom", scale = "large", returnclass = "sf")


uk_outline_bng <- uk_outline |> 
  st_transform(crs = 27700) |> 
  select(geometry)


point_sources <- point_sources_raw |> 
 # filter(Sector == "Processing & distribution of petroleum products") |> 
  select(Easting, Northing, Year, Sector, Emission, Site) |> 
  collect() |> 
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) |> 
  st_filter(uk_outline_bng)



# Making clusters to represent industrial areas as one....


coords <- st_coordinates(point_sources)

cl <- dbscan(coords, eps = 10000, minPts = 1) # 10km radius for now 

point_sources$cluster <- cl$cluster



point_sources_clustered <- point_sources |> 
  mutate(x = coords[,1], 
         y = coords[,2]) |> 
  st_drop_geometry() |>
  group_by(cluster, Year) |>
  summarise(
    total_emissions = sum(Emission),
    
    x = weighted.mean(x, Emission),
    y = weighted.mean(y, Emission),
    
    n_sources = n()
  ) |>
  st_as_sf(coords = c("x", "y"), crs = 27700) |> 
  group_by(Year) |> 
  filter(total_emissions >= quantile(total_emissions, 0.9))




plotly::ggplotly(
ggplot(point_sources_clustered) +
  geom_sf(data = uk_outline_bng, aes(geometry = geometry), fill = "lightgrey") +
  geom_sf(aes(size = total_emissions, colour = total_emissions, fill = total_emissions, text = n_sources)) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  facet_wrap(~Year) +
  coord_sf()+
  ggthemes::theme_map() +
  theme(legend.position = "top"), 
tooltip = "all"
) 
 



# For each year lets just get a look at the top 10 sources once clustered to 1 km.... 

cluster_locations <- point_sources_clustered |>
  st_transform(crs = 4326) |> 
  group_by(cluster) |>
  summarise(
    total_emissions_since_2005 = sum(total_emissions)
  )

plotly::ggplotly(
ggplot(a) +
  geom_sf(data = uk_outline_bng) +
  geom_sf(aes(geometry = geometry, size = total_emissions_since_2005, text = cluster)) , tooltip = "all"
)

leaflet(cluster_locations) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    layerId = ~cluster,
    radius = ~sqrt(total_emissions_since_2005),
    popup = ~paste("Cluster:", cluster)
  )
