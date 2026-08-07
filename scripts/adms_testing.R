
# ADMS 5 NetCDF output processing — Seasonal Filtering & Faceted Mapping of outputs!!! 

library(ncdf4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

path <- "ADMS_modelling/model_files/stanlow_test/stanlow_test.nc"   

# ---- 1. Read everything needed ---- Using the ncdf4 functions... 
nc <- nc_open(path)

x <- ncvar_get(nc, "PointX_XYZ")
y <- ncvar_get(nc, "PointY_XYZ")
conc <- ncvar_get(nc, "Dataset1")         # dims [nPoints, nGroups, nMetLines]
groups <- trimws(ncvar_get(nc, "Group"))
met_line <- trimws(ncvar_get(nc, "Met_Line"))

nc_close(nc)


# ---- 2. Parse Met_Line (format: YYYY_DDD_HH) into real dates ----
met_info <- tibble(met_line) |>
  mutate(
    year = str_sub(met_line, 1, 4) |> as.integer(),
    doy  = str_sub(met_line, 6, 8) |> as.integer(),
    hour = str_sub(met_line, 10, 11) |> as.integer(),
    date = ymd(paste0(year, "-01-01")) + days(doy - 1),
    season = case_when(
      month(date) %in% c(12, 1, 2)  ~ "DJF",
      month(date) %in% c(3, 4, 5)   ~ "MAM",
      month(date) %in% c(6, 7, 8)   ~ "JJA",
      TRUE                          ~ "SON"
    ) |>
      factor(levels = c("DJF", "MAM", "JJA", "SON"))
  )

count(met_info, season)

# ---- 3. Mask auto NA fill value ----
conc[conc == -999] <- NA

# ---- Convert to a long tidy data frame ----

df_long <- as_tibble(conc, .name_repair = "minimal") |>
  setNames(met_info$met_line) |>
  mutate(x = x, y = y) |>
  pivot_longer(
    cols = -c(x, y),
    names_to = "met_line",
    values_to = "conc"
  ) |>
  left_join(met_info, by = "met_line") 



df_long |> 
  mutate(month = month(date)) |> 
  group_by(season, x, y) |> 
  summarise(max_conc = max(conc, na.rm = T), 
            mean_conc = mean(conc, na.rm = T)) |>
  pivot_longer(cols = c(max_conc, mean_conc), names_to = "metric", values_to = "conc") |>  
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = conc), alpha = 0.6) +
  scale_fill_viridis() +
  facet_grid(cols = vars(season), rows = vars(metric))


# Add background concentration for total exposure concentration
# background_benzene <- 0.6    # ppb -> µg/m3, using your nc's conversion factor
# seasonal_means_total <- seasonal_means + background_benzene
# 
# df <- data.frame(x = x, y = y, seasonal_means_total)


# Come back to this section to look at data....

# ---- 6. Reshape to long format for faceting ----
df_long <- df |>
  pivot_longer(
    DJF:SON,
    names_to = "season",
    values_to = "conc"
  ) |>
  mutate(
    season = factor(season, levels = c("DJF", "MAM", "JJA", "SON"))
  )

# Only used to define the extent
extent_sf <- df |>
  st_as_sf(coords = c("x", "y"), crs = 27700)

tiles <- get_tiles(
  extent_sf,
  provider = "CartoDB.Positron",
  crop = TRUE
)



ggplot() +
  geom_spatraster_rgb(data = tiles) +
  geom_raster(
    data = df_long,
    aes(x, y, fill = conc),
    alpha = 0.7
  ) +
  scale_fill_viridis(name = expression(Benzene~(µg/m^3)), limits = c(0,NA)) +
  facet_wrap(~season) 


openair::windRose(met_data, type = "season")




