library(tidyverse)
library(sf)
library(plotly)
library(openair)


# Which monitoring sites should we compare? 

monitoring_sites   <- read_rds("processed_data/monitoring_sites.rds")
sites_active_years <- read_rds("processed_data/sites_active_years.rds")

ahn_locs  <- monitoring_sites |> 
  filter(network == "AHN")  |> 
  distinct(`Site Name`, .keep_all = TRUE)

nahn_locs <- monitoring_sites |> 
  filter(network == "NAHN") |> 
  distinct(`Site Name`, .keep_all = TRUE)

# ---- Distance from every automatic site to every non-automatic site ----
dist_km <- st_distance(ahn_locs, nahn_locs) |> 
  units::drop_units() / 1000

candidate_pairs <- expand_grid(
  i = seq_len(nrow(ahn_locs)),
  j = seq_len(nrow(nahn_locs))
) |>
  mutate(
    ahn_site    = ahn_locs$`Site Name`[i],
    ahn_type    = ahn_locs$`Environment Type`[i],
    nahn_site   = nahn_locs$`Site Name`[j],
    nahn_type   = nahn_locs$`Environment Type`[j],
    distance_km = dist_km[cbind(i, j)],
    same_type   = ahn_type == nahn_type
  ) |>
  select(-i, -j)

# ---- Years when both sites in each pair were running ----
years_by_site <- sites_active_years |>
  st_drop_geometry() |>
  distinct(`Site Name`, network, year)

overlap_years <- candidate_pairs |>
  select(ahn_site, nahn_site) |>
  left_join(years_by_site |> filter(network == "AHN"),
            by = c("ahn_site" = "Site Name"), relationship = "many-to-many") |>
  inner_join(years_by_site |> filter(network == "NAHN"),
             by = c("nahn_site" = "Site Name", "year")) |>
  summarise(overlap_years = n_distinct(year),
            first_year = min(year), last_year = max(year),
            .by = c(ahn_site, nahn_site))



# ---- The 3 nearest non-automatic sites for each automatic site ----
nearest_pairs <- candidate_pairs |>
  left_join(overlap_years, by = c("ahn_site", "nahn_site")) |>
  mutate(overlap_years = coalesce(overlap_years, 0L)) |>
  slice_min(distance_km, n = 3, by = ahn_site) |>
  arrange(ahn_site, distance_km)


# And then narrowing down sensible comparisons.... 


comparison_options <- nearest_pairs |>
  mutate(distance_km = round(distance_km, 1)) |>
  select(ahn_site, ahn_type, nahn_site, nahn_type, distance_km,
         same_type, overlap_years, first_year, last_year) |>
 # print(n = Inf) |> 
  filter(overlap_years > 5) |> 
  filter(distance_km < 10)



sites_to_compare <- comparison_options |> 
  select(ahn_site, nahn_site) 


# ---- 1. Load data (

# Non-automatic: one row per tube, columns: site, start_date, end_date, benzene
ahn_hourly <- read_rds("data/measured_data/ahn_hourly.rds")

nahn_tubes <- read_rds("data/measured_data/nahn_monitoring_data.rds") |> 
  mutate(site = ifelse(site == "Camden Kerbside(Swiss Cottage)", "Camden Kerbside", site))




# ---- 2. Average the hourly data over each tube's sampling period ----
paired <- nahn_tubes |>
  filter(site %in% sites_to_compare$nahn_site) |>
  mutate(period_id = row_number(),
         hours_in_period = as.numeric(difftime(end_date, start_date, units = "hours"))) |>
  inner_join(
    ahn_hourly |> filter(site %in% sites_to_compare$ahn_site), # ahn site same for both so this doesnt matter too much 
    by = join_by(start_date <= date, end_date > date)   # hours inside the tube period
  ) |>
  rename(nahn_site = site.x, ahn_site = site.y) |> 
  summarise(
    nahn         = first(benzene.x),
    ahn          = mean(benzene.y, na.rm = TRUE),
    data_capture = sum(!is.na(benzene.y)) / first(hours_in_period),
    .by = c(nahn_site, period_id, start_date, end_date)
  ) |>
  filter(data_capture >= 0.75, 
         !is.na(nahn), !is.na(ahn)) |>
  mutate(year       = year(start_date),
         difference = nahn - ahn,
         ratio      = nahn / ahn)

# ---- 3. Summary statistics per site ----
comparison_stats <- paired |>
  summarise(
    n_periods    = n(),
    mean_ahn     = mean(ahn),
    mean_nahn    = mean(nahn),
    mean_bias    = mean(difference),                # NAHN minus AHN
    pct_bias     = mean(difference) / mean(ahn) * 100,
    correlation  = cor(ahn, nahn),
    slope        = coef(lm(nahn ~ ahn))[2],
    .by = nahn_site
  )

comparison_stats

method_colours <- c("Automatic" = "#2a78d6", "Non-automatic" = "#eb6834")

# ---- 4a. Time series: both methods on the same tube periods ----
p_timeseries <- paired |>
  pivot_longer(c(ahn, nahn), names_to = "method", values_to = "benzene") |>
  mutate(method = if_else(method == "ahn", "Automatic", "Non-automatic")) |>
  ggplot(aes(x = start_date, y = benzene, colour = method,
             )) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  geom_point(aes(text = paste0("<b>", method, "</b><br>",
                               format(start_date, "%d %b %Y"), " to ",
                               format(end_date, "%d %b %Y"), "<br>",
                               "Benzene: ", round(benzene, 2), " µg m⁻³")), size = 0.6) +
  scale_colour_manual(values = method_colours, name = NULL) +
  scale_y_continuous(name = "Benzene (µg m⁻³)", limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_date(name = NULL) +
  facet_wrap(~ nahn_site, ncol = 1) +
  theme_minimal(13) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0))

# ---- 4b. Scatter: perfect agreement sits on the dashed 1:1 line ----
axis_max <- max(c(paired$ahn, paired$nahn), na.rm = TRUE) * 1.05

p_scatter <- paired |>
  ggplot(aes(x = ahn, y = nahn,
             text = paste0(format(start_date, "%d %b %Y"), "<br>",
                           "Automatic: ", round(ahn, 2), "<br>",
                           "Non-automatic: ", round(nahn, 2)))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  geom_point(aes(colour = year), size = 1.5, alpha = 0.7) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE,
              colour = "grey20", linewidth = 0.6) +
  scale_colour_viridis_c(name = "Year") +
  scale_x_continuous(name = "Automatic (µg m⁻³)", limits = c(0, axis_max), expand = c(0, 0)) +
  scale_y_continuous(name = "Non-automatic (µg m⁻³)", limits = c(0, axis_max), expand = c(0, 0)) +
  coord_equal() +
  facet_wrap(~ nahn_site) +
  theme_minimal(13) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0))

# ---- 4c. Has the agreement changed over the years? ----
p_ratio <- paired |>
  summarise(ratio = mean(nahn) / mean(ahn), n = n(), .by = c(nahn_site, year)) |>
  ggplot(aes(x = year, y = ratio,
             text = paste0("<b>", nahn_site, "</b><br>", year, "<br>",
                           "Non-automatic / Automatic: ", round(ratio, 2), "<br>",
                           "Periods: ", n))) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_line(colour = "#16317D", linewidth = 0.8) +
  geom_point(colour = "#16317D", size = 1.5) +
  scale_y_continuous(name = "Ratio (non-automatic ÷ automatic)", limits = c(0, NA)) +
  scale_x_continuous(name = NULL) +
  facet_wrap(~ nahn_site) +
  theme_minimal(13) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0))

ggplotly(p_timeseries, tooltip = "text")
ggplotly(p_scatter,    tooltip = "text")
ggplotly(p_ratio,      tooltip = "text")





nahn_hourly <- nahn_tubes |> 
  filter(site %in% c("London Bloomsbury", "Camden Kerbside")) 

ggplotly(

ahn_hourly |> 
  filter(site == "London Marylebone Road") |> 
  ggplot(aes(x= date, y = benzene)) +
  geom_point(colour = "grey40", size = 0.2) +
  geom_line(data = nahn_hourly, aes(x =end_date, y = benzene, colour = site)) 

)



plot_data <- paired |>
  pivot_longer(c(ahn, nahn), names_to = "method", values_to = "benzene") |>
  mutate(method = if_else(method == "ahn", "Automatic", "Non-automatic")) |>
  arrange(nahn_site, method, start_date) |>
  mutate(
    # ---- Gaps: start a new segment if > 31 days since the previous period ended
    gap_days = as.numeric(start_date - lag(end_date)),
    new_segment = is.na(gap_days) | gap_days > 31,
    segment = cumsum(new_segment),
    
    # ---- 12-month rolling mean, centred, based on dates (not row counts)
    mid_date = start_date + (end_date - start_date) / 2,
    rolling_mean = slide_index_dbl(
      benzene, mid_date, mean,
      .before = days(182), .after = days(182), .complete = FALSE
    ),
    periods_in_window = slide_index_int(
      benzene, mid_date, length,
      .before = days(182), .after = days(182)
    ),
    # Only keep the rolling mean when the year is at least 75% covered (~20 of 26 fortnights)
    rolling_mean = if_else(periods_in_window >= 13, rolling_mean, NA_real_),
    .by = c(nahn_site, method)
  )


ggplotly(
ggplot(plot_data, aes(x = mid_date, colour = method)) +
  # Raw fortnightly values, faded, broken at gaps
  geom_line(aes(y = benzene, group = interaction(method, segment)),
            linewidth = 0.4, alpha = 0.35) +
  geom_point(aes(y = benzene,
                 text = paste0("<b>", method, "</b><br>",
                               format(start_date, "%d %b %Y"), " to ",
                               format(end_date, "%d %b %Y"), "<br>",
                               "Benzene: ", round(benzene, 2), " µg m⁻³<br>",
                               "12-month mean: ", round(rolling_mean, 2), " µg m⁻³")),
             size = 0.5, alpha = 0.5) +
  # 12-month rolling mean, bold (gaps appear automatically where it's NA)
  geom_line(aes(y = rolling_mean, group = method), linewidth = 1.1, na.rm = TRUE) +
  scale_colour_manual(values = method_colours, name = NULL) +
  scale_y_continuous(name = "Benzene (µg m⁻³)", limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_date(name = NULL) +
  facet_wrap(~ nahn_site, ncol = 1) +
  theme_minimal(13) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0))

)

