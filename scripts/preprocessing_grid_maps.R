library(tidyverse)
library(arrow)
library(terra)
library(sf)
library(rnaturalearth)

# -----------------------------------------------------------------------------
# Pre-render gridded benzene emissions maps as PNGs, so the Shiny app can
# just display an image instead of rebuilding a raster + ggplot on every
# reactive change. Run this script manually whenever bz_all is refreshed —
# it does NOT need to run inside the Shiny app itself.
# -----------------------------------------------------------------------------

dataset_path <- "processed_data/bz_all"
output_dir <- "processed_data/gridded_maps"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

message("Connecting to dataset...")
naei_ds <- open_dataset(dataset_path)

uk_outline <- ne_countries(
  country = "United Kingdom",
  scale = "large",
  returnclass = "sf"
) |>
  st_transform(27700) |>
  select(geometry)

# -----------------------------------------------------------------------------
# The pre-computed "total" source, so the all-sources map can just filter
# to it directly instead of summing every individual source ourselves —
# that manual summation across the full dataset was the slow step.
# -----------------------------------------------------------------------------

distinct_sources <- naei_ds |> distinct(source) |> collect() |> pull(source) |> sort()
total_label <- "tota"

if (!(total_label %in% distinct_sources)) {
  stop(
    "total_label ('", total_label, "') not found among the dataset's source values: ",
    paste(distinct_sources, collapse = ", "),
    ". Update total_label above to match."
  )
}
message("Using pre-computed total layer: '", total_label, "'")

individual_sources <- setdiff(distinct_sources, total_label)
years <- naei_ds |> distinct(year) |> collect() |> arrange(year) |> pull(year)

# A single fixed colour scale (log1p), so "hot" means the same thing on
# every map — otherwise each one would rescale to its own min/max and you
# couldn't visually compare 2005 to 2020, or one source to another. This
# is a single aggregate pushed down to Arrow, not a full data pull.
global_max_log <- naei_ds |>
  summarise(max_bz = max(bz, na.rm = TRUE)) |>
  collect() |>
  pull(max_bz) |>
  log1p()

safe_name <- function(x) str_replace_all(x, "[^A-Za-z0-9]+", "_")

# -----------------------------------------------------------------------------
# Pull one year/source slice at a time. Filtering happens on the Arrow
# side before collect(), so each call only pulls the rows it needs rather
# than materialising the whole dataset. The group_by/summarise below is a
# safety net for any duplicate (x, y) rows within that slice — cheap,
# since it's operating on an already-filtered, already-small subset.
# -----------------------------------------------------------------------------

pull_grid_slice <- function(yr, src) {
  naei_ds |>
    filter(year == yr, source == src) |>
    select(x, y, bz) |>
    collect() |>
    group_by(x, y) |>
    summarise(total_bz = sum(bz, na.rm = TRUE), .groups = "drop")
}

# -----------------------------------------------------------------------------
# Rendering function — same raster-building + ggplot logic as the Shiny
# app's live version, just called in a loop and saved to disk instead of
# returned to a reactive.
# -----------------------------------------------------------------------------

render_map <- function(df, out_path, subtitle) {
  if (nrow(df) == 0) {
    message("  Skipping ", basename(out_path), " — no rows for this selection")
    return(invisible(NULL))
  }

  r <- terra::rast(
    as.data.frame(df)[, c("x", "y", "total_bz")],
    type = "xyz",
    crs = "EPSG:27700"
  )
  r[r <= 0] <- NA
  r_log <- log1p(r)
  raster_df <- as.data.frame(r_log, xy = TRUE)

  if (nrow(raster_df) == 0) {
    message("  Skipping ", basename(out_path), " — every cell is zero/NA")
    return(invisible(NULL))
  }

  p <- ggplot() +
    geom_raster(data = raster_df, aes(x = x, y = y, fill = total_bz)) +
    geom_sf(data = uk_outline, fill = NA, colour = "grey30", linewidth = 0.3, inherit.aes = FALSE) +
    scale_fill_viridis_c(
      name = "Total benzene",
      na.value = "transparent"
    ) +
    coord_sf(crs = 27700, datum = NA) +
    theme_minimal() +
    theme(axis.title = element_blank()) +
    labs(subtitle = subtitle)

  ggsave(out_path, p, width = 7, height = 8, dpi = 300, bg = "white")
  message("  Saved ", basename(out_path))
}

# -----------------------------------------------------------------------------
# 1. All-sources map, one per year — using the pre-computed total layer
# -----------------------------------------------------------------------------

message("Rendering all-sources maps, one per year (using the pre-computed total layer)...")
for (yr in years) {
  df_yr <- pull_grid_slice(yr, total_label)
  out_path <- file.path(output_dir, paste0("all_sources_", yr, ".png"))
  render_map(df_yr, out_path, subtitle = paste("All sources —", yr))
}

# -----------------------------------------------------------------------------
# 2. Per-source maps, one per year x source
# -----------------------------------------------------------------------------

message("Rendering per-source maps, one per year x source...")
for (src in individual_sources) {
  for (yr in years) {
    df_yr_src <- pull_grid_slice(yr, src)
    out_path <- file.path(output_dir, paste0(safe_name(src), "_", yr, ".png"))
    render_map(df_yr_src, out_path, subtitle = paste(src, "—", yr))
  }
}

message("Done. Images saved to: ", output_dir)
