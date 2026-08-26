# Comparing onshore emissions to onshore point source.... 


library(arrow)
library(tidyverse)
library(sf)


expanded_palette = c(
  "#2B5C8F", # Deep Blue
  "#4EA8DE", # Sky Blue
  "#66C2A5", # Mint Teal
  "#A6D854", # Soft Green
  "#FFD92F", # Warm Yellow
  "#FC8D62", # Coral Orange
  "#E78AC3", # Rose Pink
  "#8DA0CB", # Lavender Purple
  "#B39DDB", # Deep Lilac (Extra)
  "#E5C494" # Muted Sand (Extra)
)

naei_uk <- open_dataset("processed_data/bz_uk_filtered")



total_emissions_per_year <- naei_uk |>
  filter(source == "tota") |> 
  group_by(year) |> 
  summarise(total_emissions_per_year = sum(bz, na.rm = T)) |> 
  collect() 



ggplot(total_emissions_per_year) +
  geom_line(aes(x = year, y = total_emissions_per_year))




nice_labels <- c("01energypro" = "Combustion in Energy Production and Transformation", 
                 "02nonindustcom" = "Combustion in Commercial, Institutional, Residential and Agriculture", 
                 "03industcom" = "Combustion in Industry", 
                 "04prodproces"  = "Production Processes",
                 "05offshor"  = "Extraction and Distribution of Fossil Fuels",
                 "06solvents" = "Solvent Use",
                 "07roadtran" = "Road Transport",
                 "08othertran" = "Other Transport and Mobile machinery",
                 "09wast"  = "Waste Treatment and Disposal",
                 "tota" = "Total emissions (sum of all the grids above and point sources)")




point_sources <- read_rds("processed_data/point_sources.rds")



total_ps_emissions_per_year <- point_sources |> 
  st_drop_geometry() |> 
  group_by(Year) |> 
  summarise(total_point_source_emissions_per_year = sum(Emission, na.rm = T))


point_sources_vs_tot <- total_emissions_per_year |> 
  left_join(total_ps_emissions_per_year, join_by(year == Year)) |> 
  mutate(pct_of_total = (total_point_source_emissions_per_year/ total_emissions_per_year) * 100 ) |> 
  arrange(year) |> 
  mutate(relative_to_baseline_ps = total_point_source_emissions_per_year/total_point_source_emissions_per_year[1], 
         relative_to_baseline_total = total_emissions_per_year/total_emissions_per_year[1])
  

ggplot(point_sources_vs_tot, aes(x = year, y = pct_of_total)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(name = "Percentage of Total Emissions (%)", expand = c(0,0), limits = c(0,15)) +
  scale_x_continuous(name = "Year") +
  ggtitle("Point sources as a percentage of total emissions") +
  theme_minimal(16) +
  theme(axis.line = element_line(), 
        legend.position = "top")


ggsave("plots/basic_plots/pct_of_total.png")

 
point_sources_vs_tot |> 
  pivot_longer(cols = total_emissions_per_year:total_point_source_emissions_per_year, values_to = "emissions", names_to = "metric") |> 
  ggplot() +
  geom_line(aes(x = year, y = emissions, colour = metric)) +
  geom_point(aes(x = year, y = emissions, colour = metric)) +
  scale_y_continuous(name = "Emissions (Tonnes)", expand = c(0,0), limits = c(0,17000)) +
  scale_x_continuous(name = "Year") +
  scale_colour_manual(values = c("total_emissions_per_year" = "#B39DDB", "total_point_source_emissions_per_year" = "#A6D854"), 
                      labels = c("total_emissions_per_year" = "Total", "total_point_source_emissions_per_year" = "Point Sources")) +
  theme_minimal(16) +
  theme(axis.line = element_line(), 
        legend.position = "top", 
        legend.title = element_blank())



ggsave("plots/basic_plots/total_versus_emissions.png")



point_sources_vs_tot |> 
  pivot_longer(cols = relative_to_baseline_ps:relative_to_baseline_total, values_to = "percentage", names_to = "metric") |> 
  ggplot() +
  geom_line(aes(x = year, y = percentage * 100, colour = metric)) +
  geom_point(aes(x = year, y = percentage * 100, colour = metric)) +
  scale_y_continuous(name = "Percentage relative to 2005 baseline (%)", expand = c(0,0), limits = c(0,102)) +
  scale_x_continuous(name = "Year") +
  scale_colour_manual(values = c("relative_to_baseline_total" = "#B39DDB", "relative_to_baseline_ps" = "#A6D854"), 
                      labels = c("relative_to_baseline_total" = "Total", "relative_to_baseline_ps" = "Point Sources")) +
  theme_minimal(16) +
  theme(axis.line = element_line(), 
        legend.position = "top", 
        legend.title = element_blank())


ggsave("plots/basic_plots/total_versus_emissions_relative_to_baseline.png")

# Okay so we havent particularly seen anything interesting in these trend but maybe it would be good to see if we are concentrating our emissions into less and less point sources....

point_sources <- read_rds("processed_data/point_sources.rds")


point_sources |> 
  st_drop_geometry() |> 
  group_by(Year) |> 
  summarise(sites_per_year = n(), total_emissions = sum(Emission, na.rm = T) )|> 
  pivot_longer(cols = sites_per_year:total_emissions, names_to = "metric", values_to = "values") |> 
  ggplot() +
  geom_line(aes(x = Year, y = values)) +
  scale_y_continuous(limits = c(0,NA), name = NULL ) +
  facet_wrap(~metric, scales = "free", labeller = as_labeller(c(
    sites_per_year = "Number of Point Sources",
    total_emissions = "Emissions from Point Sources (tonnes)"
  ))) +
  theme_minimal(16) +
  theme(axis.line = element_line(), 
        legend.position = "top", 
        legend.title = element_blank())



ggsave("plots/basic_plots/ps_numbers.png")


new_sources <- point_sources |> 
  st_drop_geometry() |> 
  group_by(PlantID, Site) |> 
  summarise(
    first_year = min(Year, na.rm = TRUE),
    last_year = max(Year, na.rm = TRUE), 
    mean_emissions = mean(Emission, na.rm = T)
  ) |> 
  mutate(new_source_flag = ifelse(first_year > 2019, "new_source", "old_source")) |> 
  select(new_source_flag, PlantID)


write_csv(new_sources, "data/processed_data/new_naei_post_2020_sources.csv")


point_sources |> 
  left_join(new_sources) |> 
  group_by(Year, new_source_flag) |> 
  summarise(totes_emish = sum(Emission, na.rm = T)) |> 
  ggplot() +
  geom_area(aes(x = Year, y = totes_emish, fill = new_source_flag), alpha = 0.6) +
  geom_line(
    aes(x = Year, y = totes_emish),
    data = \(x) x |> 
      group_by(Year) |> 
      summarise(totes_emish = sum(totes_emish)),
    linewidth = 1
  ) +
  scale_fill_manual(values = c("#E78AC3", "#8DA0CB"))  +
  scale_x_continuous(name = "Year", expand = c(0,0)) +
  scale_y_continuous(name = "Total Emissions (tonnes)", limits = c(0,2000), expand = c(0,0)) +
  theme_minimal(16) +
  theme(axis.line = element_line(), 
        legend.position = "top", 
        legend.title = element_blank())


ggsave("plots/basic_plots/new_versus_old_sources.png")
