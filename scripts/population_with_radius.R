# For my population grid, I want to supply a point coordinate and find out how many people are in that sphere of radius x 
library(raster)
library(sf)
library(tidyverse)
library(rnaturalearth)
library(terra)
library(exactextractr)
library(sfarrow)




pop_grid <- rast("data/geography/population_per_1km2/data/uk_residential_population_2021.tif") 
small_area_geographies <- st_read_parquet("data/geography/uk-small-area-lsoa-soa-dz-apr-2021.parquet")



# Make into a function.... 

population_with_radius <- function(x = 344190, 
                                   y = 375060, 
                                   point_crs = 27700, 
                                   population_radius_km = 10, 
                                   site_name = NA){
  
  
  point_coordinates <- tibble(x = x, y = y) |> 
    st_as_sf(coords = c("x", "y"), crs = point_crs) |> 
    st_transform(crs(pop_grid))
  
  
  buffer_vector <- st_buffer(point_coordinates, population_radius_km * 1000) |> 
    vect()
  
  
  pop_grid |> 
    crop(buffer_vector) |> 
    mask(buffer_vector) |> 
    plot()
  
  
  
  
  pop_within_10km <- extract(
    pop_grid,
    buffer_vector,
    weights = TRUE,
    fun = sum,
    na.rm = TRUE
  ) |> 
    summarise(population = round(pop_aw, 0))  # Make to the nearest whole number 
  
  
  if (is.na(site_name) == F){
    pop_within_10km <-  pop_within_10km |> 
      mutate(site = site_name)
  }
  
  
  return(pop_within_10km)
  
}


# Wilton 
population_with_radius(x = 457290, y = 522540, population_radius_km = 10, site_name = "wilton")


# South Killingholme
population_with_radius(515660, 416590, population_radius_km = 10)


population_with_radius(population_radius_km = 20)



# Would be good to also attach so demographic information about the area.... 
# To do this will need the small area geographies... 


# Okay now making into a function! Can combine with previous! 

population_profile_within_radius <- function(
    x = 344190, 
    y = 375060, 
    population_radius_km = 10,
    population_grid = pop_grid,
    lsoa_geographies = small_area_geographies,
    imd_data = IMD::imd_england_lsoa,
    point_crs = 27700, 
    site_name = NA, 
    plot = F
) {
  
  point_coordinates <- tibble(x = x, y = y) |>
    st_as_sf(coords = c("x", "y"), crs = point_crs)
  
  buffer <- point_coordinates |>
    st_transform(st_crs(lsoa_geographies)) |>
    st_buffer(population_radius_km * 1000)
  
  small_area_crop <- lsoa_geographies |>
    st_crop(st_bbox(buffer)) |>
    st_intersection(buffer)
  
  polygons_with_population <- small_area_crop |> 
    mutate(population = exact_extract(
      pop_grid,
      small_area_crop,
      function(values, coverage_fraction) {
        sum(values * coverage_fraction, na.rm = TRUE)
      }
    )) 
  

    with_imd_data <- polygons_with_population |>
      left_join(imd_data,
                by = c("areacode" = "lsoa_code")) |> 
      mutate(site = site_name)
  
   
    if (plot == T){
    
    
   plt <-  with_imd_data |> 
      group_by(IMD_decile) |> 
      summarise(total_pop_per_decile = sum(population)) |> 
      mutate(pct_pop = total_pop_per_decile/ sum(total_pop_per_decile)) |> 
      ggplot(aes(x = IMD_decile, y = pct_pop * 100, fill = factor(IMD_decile))) +
      geom_col() +
      scale_x_continuous(breaks = seq(1,10,1), limits = c(0.5,10.5)) +
      scale_y_continuous(name = "Percentage of Population in Decile (%)") +
      scale_fill_brewer(palette = "PiYG", direction = 1) +
      theme_minimal()
   
   print(plt)
   
   
   
   spatial_plot <- with_imd_data |> 
     ggplot() +
     geom_sf(aes(geometry = geom, fill = factor(IMD_decile))) +
     geom_sf(data = point_coordinates, colour = "darkgrey") +
     scale_fill_brewer(palette = "PiYG", direction = 1)
   
   
   print(spatial_plot)
    }
   
   return(with_imd_data) 
}



stanlow_10k <- population_profile_within_radius(x = 344190,
                                               y = 375060, 
                                               population_radius_km = 10,
                                               population_grid = pop_grid,
                                               lsoa_geographies = small_area_geographies,
                                               imd_data = IMD::imd_england_lsoa,
                                               point_crs = 27700, site_name = "Stanlow")



# South Killingholme
south_killingholme <- population_profile_within_radius(515660, 416590, population_radius_km = 10, site_name = "South Killingholme")

# Wilton 
wilton <- population_profile_within_radius(x = 457290, y = 522540, population_radius_km = 10, site = "Wilton")

# Fawley  443800, 105600
fawley <- population_profile_within_radius(x = 443800, y = 105600, population_radius_km = 10, site = "Fawley")


# Workington Mill  300475, 531230
workington <- population_profile_within_radius(x = 300475, y = 531230, population_radius_km = 10, site = "Workington")



# Will be good to look at the how the distribution compares to a uniform average? But would we even anticipate a normal distribution? 


rbind(stanlow_10k, 
      south_killingholme, 
      fawley, 
      wilton, 
      workington) |> 
  filter(!is.na(IMD_decile)) |> 
  group_by(site, IMD_decile) |> 
  summarise(population = sum(population)) |> 
  mutate(pop_pct = population/sum(population)) |> 
  group_by(site) |>
  mutate(prop = population / sum(population)) |> 
  group_by(IMD_decile) |>
  summarise(
    mean_prop = mean(prop),
    sd_prop = sd(prop)
  ) |> 
  ggplot(aes(x = IMD_decile, y = mean_prop, fill = IMD_decile)) +
  geom_col() +
  #geom_errorbar(aes(ymin = mean_prop - sd_prop, ymax = mean_prop + sd_prop), colour = "pink") +
  scale_fill_distiller(palette = "Set1")




# Now lets test on the pollutant inventory all data! 


# read in and tidy the dataset first.... 

pollutant_data_raw <- read_csv("data/EA_PI/2023_pollution_inventory.csv")



colnames(pollutant_data_raw) <- pollutant_data_raw[9,]




pollutant_data <- pollutant_data_raw |> 
  janitor::clean_names() |> 
  slice(-(1:9)) |> 
  rename(easting = na, northing = na_2, reporting_threshold = na_3)




pollutant_data |> 
  group_by(substance_name) |> 
  summarise(total_emissions = sum(as.numeric(quantity_released_kg,  na.rm = T), na.rm = T)) |> 
  ggplot() +
  geom_col(aes(x = log(total_emissions), y = reorder(substance_name, total_emissions)))
 


# Okay so now we want to be able to feed into the function our data from the csv file 


benzene_data <- pollutant_data |> 
 filter(substance_name == "Benzene") |> 
 filter(route_name == "Air") |> 
 filter(is.na(as.numeric(quantity_released_kg)) == F) |> 
  mutate(quantity_released_kg = as.numeric(quantity_released_kg)) |> 
  arrange(desc(quantity_released_kg))




pop_data <- map_dfr(
  seq_len(nrow(benzene_data)),
  \(i) population_profile_within_radius(
    x = benzene_data$easting[i],
    y = benzene_data$northing[i],
    population_radius_km = 5,
    site_name = benzene_data$operator_name[i]
  )
)


emissions_data <- benzene_data |> 
  dplyr::select(operator_name, quantity_released_kg)



pop_data |> 
  left_join(emissions_data, join_by(site == operator_name)) |> 
  mutate(weighted_population = population * quantity_released_kg) |> 
  ggplot() +
  geom_col(aes(x = IMD_decile, y = weighted_population, fill = site)) +
  scale_x_continuous(breaks = seq(1,10,1))


