# Downloading the Benzene data 
library(arrow)
library(dplyr)
library(ggplot2)

remotes::install_github("lucy-web-0812/lucyr")


library(lucyr)
lucyr::download_naei_emissions(years = 2005:2023, pollutant = "bz", source_type = "all", include_point_sources = T)



# Get a overall plot of what this looks like....

# Point to the directory containing the partitioned folders
dataset_path <- "processed_data/bz_all"

# Connect to the dataset 
naei_ds <- open_dataset(dataset_path)


# Quick plot of emissions timeline

emissions_timeline <- naei_ds |>
  group_by(year, source) |>
  summarise(total_emissions = sum(bz, na.rm = TRUE),
            .groups = "drop") |>
  collect() # Pull this into R memory


ggplot(emissions_timeline,
       aes(x = year, y = total_emissions, colour = source, group = source)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2005, 2023, by = 2))



