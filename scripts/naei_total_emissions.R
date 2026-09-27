# NAEI Totals data 

library(tidyverse)
library(plotly)


expanded_palette = c(
  "#4EA8DE", # Sky Blue
  "#FFD92F", # Warm Yellow
  "#E78AC3", # Rose Pink
  "#66C2A5", # Mint Teal
  "#FC8D62", # Coral Orange
  "#8DA0CB", # Lavender Purple
  "#B39DDB", # Deep Lilac (Extra)
  "#E5C494", # Muted Sand (Extra)
  "#2B5C8F",
  "#A6D854", # Soft Green
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
  mutate(benzene_emission = as.numeric(benzene_emission),
         year = as.numeric(year)) |>
  mutate(
    mid_category = case_when(
      str_detect(Source, "^Road transport") ~ "Road transport",
      str_detect(Source, "^Domestic|^Household|^House") ~ "Domestic",
      str_detect(Source, "^Shipping|vessel") ~ "Shipping",
      str_detect(Source, "^Railways|^Rail -|boats|watercraft|^Aircraft")  ~ "Other transport",
      str_detect(Source, "^NRMM|^Forest") ~ "Non-road mobile machinery",
      
      str_detect(
        Source,
        "^Refineries|Petrol station|Petrol terminal|Petroleum processes"
      )  ~ "Refineries & Petrol distribution",
      str_detect(
        Source,
        "Upstream (Gas|Oil) Production|Gas Terminal|^Oil Terminal|Gas leakage|Gas production|^Gas terminal|Oil terminal: fuel combustion"
      ) ~ "Oil & gas extraction/terminals",
      str_detect(
        Source,
        "^Blast furnaces|^Cement|^Coke production|^Iron and steel|^Lime production|^Sinter production|^Non-Ferrous Metal"
      ) ~ "Heavy industry (metals, cement, coke)",
      str_detect(Source, "^Power stations|Collieries|Nuclear fuel") ~ "Power generation",
      str_detect(Source, "^Chemical|Solvent and oil recovery|Coal tar and bitumen processes|Solid smokeless fuel production") ~ "Chemical Industry",
      str_detect(Source, "^Incineration|Landfill") ~ "Waste & Incineration",
      str_detect(Source, "combustion|Autogenerators") ~ "Manufacturing & Commercial Combustion",
      TRUE ~ "Other industrial/miscellaneous"
    )
  )

naei_totals <- naei_totals |> 
  mutate(final_category = case_when(
    mid_category %in% c("Power generation", "Oil & gas extraction/terminals") ~ "Energy industries",
    mid_category %in% c("Manufacturing & Commercial Combustion", "Heavy industry (metals, cement, coke)") ~ "Manufacturing & heavy industry",
    TRUE ~ mid_category
  ))





yearly_totals <- naei_totals |> 
  group_by(year) |> 
  summarise(total_emissions = sum(benzene_emission, na.rm = T))


yearly_totals|> 
  ggplot() +
  geom_point(aes(x = year, y = total_emissions), size = 0.8) +
  geom_line(aes(x = year, y = total_emissions)) +
  scale_y_continuous(name = "Benzene Emissions (kilotonnes)", expand = c(0,0), breaks = seq(0,60,10), limits = c(0,65)) +
  scale_x_continuous(name = "Year", expand = c(0,0), limits = c(1989.5, 2025.5), breaks = seq(1990, 2026,5), minor_breaks = seq(1990,2025,1))  +
  theme_minimal(18) +
  theme(legend.position = "top", 
        axis.line = element_line(), 
        axis.ticks = element_line()) 


write_csv(yearly_totals, "data/basic_benzene_data_for_ally/total_benzene_emissions.csv")


ggsave("plots/basic_plots/total_benzene_emissions.png", height = 12, width = 18, units = "cm", dpi = 600)



# Now looking by category.... 

# 1. Rank categories by their 1990 emissions (biggest first)
category_order <- naei_totals |>
  filter(!is.na(final_category), year == 1990) |>
  group_by(final_category) |>
  summarise(total_1990 = sum(benzene_emission, na.rm = TRUE)) |>
  arrange(desc(total_1990)) |>
  pull(final_category)


category_colours <- setNames(expanded_palette[seq_along(category_order)], category_order)



naei_totals |>
  filter(!is.na(final_category)) |>
  group_by(year, final_category) |>
  summarise(total_emissions = sum(benzene_emission, na.rm = T)) |>
  mutate(final_category = factor(final_category, levels = rev(category_order))) |>
  ggplot() +
  geom_col(
    aes(
      x = year,
      y = total_emissions,
      fill = final_category,
      colour = final_category
    ),
    alpha = 0.6,
    position = "fill", 
    width = 0.8
  ) +
  scale_y_continuous(name = "Percentage of Emissions", expand = c(0,0), labels = scales::label_percent()) +
  scale_x_continuous(name = "Year", expand = c(0,0), breaks = seq(1990, 2026,5), minor_breaks = seq(1990,2025,1))  +
  scale_fill_manual(values = category_colours, name = "") +
  scale_colour_manual(values = category_colours, name = "") +
  theme_minimal(18) +
  theme(legend.position = "top", 
        axis.line = element_line(),
        axis.ticks = element_line(), 
        legend.text = element_text(size = 12)) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 4), colour = guide_legend(reverse = TRUE))

ggsave("plots/basic_plots/benzene_emissions_by_source_pct.png", height = 18, width = 22, units = "cm", dpi = 600)

 

naei_totals |>
  filter(!is.na(final_category)) |>
  group_by(year, final_category) |>
  summarise(total_emissions = sum(benzene_emission, na.rm = T)) |>
  mutate(final_category = factor(final_category, levels = rev(category_order))) |>
  ggplot() +
  geom_area(aes(
    x = year,
    y = total_emissions,
    fill = final_category,
    colour =  final_category
  ),
  alpha = 0.6) +
  scale_y_continuous(name = "Benzene Emissions (kilotonnes)",
                     expand = c(0, 0),
                     breaks = seq(0, 60, 10)) +
  scale_x_continuous(name = "Year", expand = c(0, 0)) +
  scale_fill_manual(values = category_colours, name = "") +
  scale_colour_manual(values = category_colours, name = "") +
  theme_minimal(18) +
  theme(legend.position = "top", 
        axis.line = element_line(),
        axis.ticks = element_line(), 
        legend.text = element_text(size = 12)) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 4), colour = guide_legend(reverse = TRUE))


ggsave("plots/basic_plots/benzene_emissions_by_source.png", height = 18, width = 22, units = "cm", dpi = 600)




# Comparing 1990 and 2023


pie_data <- naei_totals |>
  filter(!is.na(final_category), year %in% c(1990, 2023)) |>
  mutate(final_category = factor(final_category, levels = category_order)) |>
  group_by(year, final_category) |>
  summarise(total = sum(benzene_emission, na.rm = TRUE), .groups = "drop") |>
  group_by(year) |>
  mutate(share = total / sum(total),
         year_label = paste0(year, " (", round(sum(total), 1), " kt total)")) |>
  ungroup()


centre_labels <- pie_data |>
  group_by(year) |>
  summarise(total = sum(total), .groups = "drop") |>
  mutate(label = paste0(format(round(total, 1), nsmall = 1), "kt"))



pie_labels <- pie_data |>
  arrange(year, desc(final_category)) |>
  group_by(year) |>
  mutate(ymax = cumsum(share),
         ymin = ymax - share,
         mid  = (ymin + ymax) / 2) |>
  ungroup() |>
  mutate(label  = round(share * 100,0),
         inside = share >= 0.05)  


ggplot(pie_data, aes(x = 1.5, y = share, fill = final_category)) +
  geom_col(width = 1, colour = "white", alpha = 0.8) +
  # only label slices big enough to fit text
  geom_text(data = filter(pie_labels, inside),
            aes(x = 1.5, y = mid, label =  paste0(label, "%")),
            size = 4, inherit.aes = FALSE) +
  
  # small slices: short leader line + label outside the ring
  geom_segment(data = filter(pie_labels, !inside),
               aes(x = 2.0, xend = 2.1, y = mid, yend = mid),
               colour = "black", inherit.aes = FALSE) +
  geom_text(data = filter(pie_labels, !inside),
            aes(x = 2.25, y = mid, label = label),
            size = 3, inherit.aes = FALSE) +
  geom_text(data = centre_labels,
            aes(x = 0.5, y = 0, label = label),
            inherit.aes = FALSE, size = 5, fontface = "bold", lineheight = 0.9) +
  coord_polar(theta = "y") +
  facet_wrap(~year) +
  scale_fill_manual(values = category_colours, name = "") +
  scale_x_continuous(limits = c(0.5,2.4)) +
  theme_void(18) +
  theme(legend.text = element_text(size = 12),
        strip.text = element_text(size =20), 
        legend.position = "bottom") +
  guides(fill = guide_legend(reverse = FALSE, nrow = 4))


ggsave("plots/basic_plots/pie_chart_comparison.png", dpi = 600)



naei_totals |> 
  filter(year == 2023) |> 
  group_by(final_category) |> 
  summarise(emissions = sum(benzene_emission, na.rm = T)) |> 
  arrange(desc(emissions)) 



naei_totals |> 
  group_by(Source, final_category) |> 
    summarise(emissions = sum(benzene_emission, na.rm = T)) |> 
  write_csv("data/basic_benzene_data_for_ally/groupings_total_source.csv")


