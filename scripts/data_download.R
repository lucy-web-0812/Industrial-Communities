# Downloading the Benzene data 
library(arrow)
library(dplyr)
library(ggplot2)
library(sf)

remotes::install_github("lucy-web-0812/lucyr")


library(lucyr)
lucyr::download_naei_emissions(years = 2005:2023, pollutant = "bz", source_type = "all", include_point_sources = T)



# Get a overall plot of what this looks like....

# Point to the directory containing the partitioned folders
dataset_path <- "processed_data/bz_all"

# Connect to the dataset 
naei_ds <- open_dataset(dataset_path)



uk_outline <- rnaturalearth::ne_countries(country = "United Kingdom", scale = "large", returnclass = "sf")

uk_outline_bng <- uk_outline |> 
  st_transform(crs = 27700) |> 
  select(geometry)


# Create the UK outline grid


grid_2023 <- naei_ds |> 
  filter(
    year == 2023,
    source == "01energypro"
  ) |> 
  select(x, y) |> 
  collect()

# Filter to the UK outline 

uk_grid <- grid_2023 |> 
  st_as_sf(
    coords = c("x", "y"),
    crs = 27700
  ) |> 
  st_filter(uk_outline_bng)


# All the UK X and Y coordinates within the outline....

uk_xy <- uk_grid |> 
  st_coordinates() |> 
  as.data.frame() |> 
  setNames(c("x", "y"))



# Lets see if we can clip to within the uk outline 

naei_uk <- naei_ds |> 
  semi_join(uk_xy, by = c("x", "y"))


# and save this other dataset.... 

write_dataset(
  naei_uk,
  path = "processed_data/bz_uk_filtered",
  format = "parquet",
  partitioning = "year"
)






