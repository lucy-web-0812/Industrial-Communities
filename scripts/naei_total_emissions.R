# NAEI Totals data 

library(tidyverse)
library(plotly)


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
  "#E5C494", # Muted Sand (Extra)
  "pink"
)


naei_totals_raw <- read_csv("data/naei_inventory_total/naei_uk_emissions_data_benzene.csv")



NFR_codes <- read_csv("data/naei_inventory_total/NFRcodes_descriptions.csv")

naei_totals <- naei_totals_raw |>
  pivot_longer(
    cols = c(`1990`:`2023`),
    names_to = "year",
    values_to = "benzene_emission"
  ) |>
  mutate(benzene_emission = as.numeric(benzene_emission), year = as.numeric(year)) |>
  mutate(code_2 = substr(`NFR/CRT Group`, 1,2)) |> 
  mutate(code_3 = substr(`NFR/CRT Group`, 1,3)) |> 
  mutate(code_4 = substr(`NFR/CRT Group`, 1,4)) |> 
  mutate(overall_category = 
           case_when(
             code_3 == "1A1" ~ "Energy Industries", 
             code_3 == "1A2" ~ "Manufacturing Industries and Construction",
             code_3 == "1A3" ~ "Other Transport", 
             code_3 %in% c("1A4", "1A5") ~ "Other Fuel Combustion", 
             code_2 == "1B" ~ "Other", 
             code_2 %in% c("2B", "2C", "2G", "2H") ~ "Other",
             code_2 %in% c("3B", "3D", "3F") ~ "Other", 
             code_2 %in% c("5C", "5E") ~ "Other", 
             code_2 %in% c("6A") ~ "Other",
             code_2 == "0" ~ "Other", 
             `NFR/CRT Group` %in% c("z_1A3ai(ii)", "z_1A3aii(ii)", "z_1A3di(i)") ~ "Other Transport", 
             `NFR/CRT Group` %in% c("z_11B", "z_11C")  ~ "Other"
           )) |> 
  mutate(overall_category = ifelse(code_4 == "1A3b", "Road Transport", overall_category)) 
 

naei_totals |>
  filter(!is.na(overall_category)) |> 
  group_by(year, overall_category) |> 
  summarise(total_emissions = sum(benzene_emission, na.rm = T)) |> 
  ggplot() +
  geom_area(aes(x = year, y = total_emissions, fill = overall_category, colour = overall_category), alpha = 0.6) +
  scale_y_continuous(name = "Benzene Emissions (kilotonnes)", expand = c(0,0), breaks = seq(0,60,10)) +
  scale_x_continuous(name = "Year", expand = c(0,0)) +
  scale_fill_manual(values = expanded_palette[c(1,3,4,5,7,9)], name = "") +
  scale_colour_manual(values = expanded_palette[c(1,3,4,5,7,9)], name = "") +
  theme_minimal(12) +
  theme(legend.position = "top")


ggsave("plots/basic_plots/benzene_emissions.png")



per_year_pcts <- naei_totals |> 
  filter(!is.na(overall_category)) |> 
  group_by(year, overall_category) |> 
  summarise(total_emissions = sum(benzene_emission, na.rm = T)) |> 
  ungroup() |> 
  group_by(year) |> 
  mutate(pct_emissions = total_emissions / sum(total_emissions, na.rm = T))


ggplot(per_year_pcts) +
  geom_area(aes(x = year, y = pct_emissions, fill = overall_category, colour = overall_category, group = overall_category), alpha = 0.7) +
  scale_y_continuous(name = "Pct Emissions", expand = c(0,0), seq(0,1,0.1)) +
  scale_x_continuous(name = "Year", expand = c(0,0)) +
  scale_fill_manual(values = expanded_palette[c(1,3,4,5,7,9)], name = "") +
  scale_colour_manual(values = expanded_palette[c(1,3,4,5,7,9)], name = "") +
  theme_minimal(12) +
  theme(legend.position = "top")


ggsave("plots/basic_plots/benzene_emissions_pct.png")
