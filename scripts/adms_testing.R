
# ADMS 5 NetCDF output processing — Seasonal Filtering & Faceted Mapping of outputs!!! 

library(ncdf4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(stringr)
library(lubridate)
library(sf)
library(ggspatial)


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



summary_metrics <- df_long |> 
  mutate(month = month(date)) |> 
  group_by(season, x, y) |> 
  summarise(max_conc = max(conc, na.rm = T), 
            mean_conc = mean(conc, na.rm = T)) |>
  pivot_longer(cols = c(max_conc, mean_conc), names_to = "metric", values_to = "conc") 
  

ggplot(summary_metrics) +
  geom_tile(aes(x = x, y = y, fill = conc), alpha = 0.6) +
  scale_fill_viridis() +
  facet_grid(cols = vars(season), rows = vars(metric))


summary_sf <- summary_metrics |> 
  filter(metric == "mean_conc") |> 
  mutate(
    xmin = x - 100,
    xmax = x + 100,
    ymin = y - 100,
    ymax = y + 100
  ) |> 
  rowwise() |> 
  mutate(
    geometry = st_sfc(
      st_polygon(list(
        matrix(
          c(
            xmin, ymin,
            xmax, ymin,
            xmax, ymax,
            xmin, ymax,
            xmin, ymin
          ),
          ncol = 2,
          byrow = TRUE
        )
      )),
      crs = 27700
    )
  ) |> 
  ungroup() |> 
  st_as_sf() |> 
  st_transform(3857)



summary_sf |> 
  ggplot() +
  annotation_map_tile(type = "osm") +
  geom_sf(
    aes(fill = conc),
    colour = NA,
    alpha = 0.5
  ) +
  scale_colour_viridis_c() +
  scale_fill_viridis_c() +
  facet_wrap(~season)



openair::windRose()
