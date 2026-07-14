library(shiny)
library(tidyverse)
library(sf)
library(arrow)
library(dbscan)
library(rnaturalearth)
library(leaflet)
library(scales)
library(plotly)

# -----------------------------------------------------------------------------
# Load data once at app start (not inside server/reactive — this is the
# expensive part, so we only want to pay for it once per app session, not
# once per reactive re-evaluation)
# -----------------------------------------------------------------------------

point_sources_raw <- open_dataset("processed_data/bz_point_sources")

uk_outline <- ne_countries(
  country = "United Kingdom",
  scale = "large",
  returnclass = "sf"
) |>
  st_transform(27700) |>
  select(geometry)

uk_outline_lat_long <- uk_outline |>
  st_transform(4326)

point_sources_all <- point_sources_raw |>
  select(Site, Sector, Year, Emission, Easting, Northing) |>
  collect() |>
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) |>
  st_filter(uk_outline)

sector_choices <- point_sources_all |>
  st_drop_geometry() |>
  distinct(Sector) |>
  arrange(Sector) |>
  pull(Sector)

year_range_full <- range(point_sources_all$Year, na.rm = TRUE)


total_by_sector_by_year <- point_sources_all |> 
  st_drop_geometry() |> 
  group_by(Sector, Year) |> 
  summarise(total_emissions = sum(Emission)) |> 
  ungroup() |> 
  group_by(Year) |> 
  mutate(total_emissions_pct = total_emissions/sum(total_emissions) * 100) |> 
  mutate(Sector = ifelse(total_emissions_pct < 1, "Other", Sector))


# -----------------------------------------------------------------------------
# Monitoring network data (NAHN benzene monitoring sites)
# -----------------------------------------------------------------------------

network_locations <- read_csv("data/measured_data/NAHN_locations.csv") |>
  select(c(`Site Name`, Latitude, Longitude)) |>
  rename(lng = Longitude, lat = Latitude) |>
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

monitoring_data_raw <- read_csv("data/measured_data/NAHN_measured_data_all.csv", col_names = FALSE)
names(monitoring_data_raw) <- c("col1", "col2", "col3", "col4")

monitoring_data <- monitoring_data_raw |>
  mutate(
    year = str_extract(col1, "(?<=Data for year )\\d{4}")
  ) |>
  fill(year)

# Site names occur immediately after "Multi-day data"
site_rows <- which(monitoring_data$col1 == "Multi-day data")
monitoring_data$site <- NA_character_
monitoring_data$site[site_rows + 1] <- monitoring_data$col1[site_rows + 1]

# Carry site names down until the next one
monitoring_data <- monitoring_data |>
  fill(site)

# Keep only rows beginning with a date
benzene_monitoring_data <- monitoring_data |>
  filter(str_detect(col1, "^\\d{2}/\\d{2}/\\d{4}$")) |>
  transmute(
    site,
    year = as.integer(year),
    start_date = dmy(col1),
    end_date = dmy(col2),
    benzene = as.numeric(col3),
    status_units = col4
  )

# Names in the two files don't necessarily match character-for-character
# (whitespace, punctuation) — this keeps only monitoring sites we can
# actually place on the map, and warns about any that don't match so it's
# obvious rather than silently dropped.
unmatched_sites <- setdiff(unique(benzene_monitoring_data$site), network_locations$`Site Name`)
if (length(unmatched_sites) > 0) {
  warning(
    "Monitoring sites with data but no matching location: ",
    paste(unmatched_sites, collapse = ", ")
  )
}

site_choices <- sort(unique(network_locations$`Site Name`))


# -----------------------------------------------------------------------------
# Gridded emissions data (bz_all — 1km OSGB grid, all sources)
# -----------------------------------------------------------------------------

# Kept as an open Arrow connection rather than collected in full: this is a
# UK-wide 1km grid across multiple years and sources, so we only want to
# pull one year/source-selection's worth into memory at a time, inside the
# reactive below — not the whole dataset at app start.
naei_ds <- open_dataset("processed_data/bz_all")

raster_year_summary <- naei_ds |>
  summarise(min_year = min(year, na.rm = TRUE), max_year = max(year, na.rm = TRUE)) |>
  collect()


raster_year_range <- substr(seq(raster_year_summary$min_year, raster_year_summary$max_year), 1, 4)

raster_source_choices <- naei_ds |>
  distinct(source) |>
  collect() |>
  arrange(source) |>
  pull(source)


# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- navbarPage(
  
  
  title = "UK Benzene Dashboard",
  
  tabPanel("On Shore Point Emission Sources",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      helpText(
        "Sites are grouped into clusters using DBSCAN on their locations.",
        "Adjust the radius below to make clusters coarser (larger radius,",
        "fewer bigger clusters) or finer (smaller radius, more separate",
        "clusters)."
      ),
      sliderInput(
        "eps",
        "Clustering radius (km)",
        min = 1, max = 50, value = 10, step = 1
      ),
      selectInput(
        "sectors",
        "Sector(s)",
        choices = sector_choices,
        selected = "Processing & distribution of petroleum products",
        multiple = TRUE
      ),
      sliderInput(
        "year_range",
        "Year range",
        min = year_range_full[1], max = year_range_full[2],
        value = year_range_full,
        step = 1, sep = ""
      ),
      sliderInput(
        "top_pct",
        "Show top X% of clusters by total emissions",
        min = 10, max = 100, value = 100, step = 5
      ),
      hr(),
      helpText("Click a marker on the map to see its emissions trend and top sites.")
    ),
    mainPanel(
      width = 9,
      leafletOutput("map", height = 500),
      br(),
      uiOutput("cluster_title"),
      plotlyOutput("trend_plot", height = 250),
      tableOutput("site_table"), 
      plotlyOutput("total_by_sector_plot")
      )
    )
  ),


  tabPanel(
    "Monitoring",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        helpText(
          "Ambient benzene monitoring locations (NAHN network).",
          "Click a marker on the map, or pick a site below, to see its",
          "measured benzene time series."
        ),
        selectInput(
          "monitoring_site",
          "Site",
          choices = c("(click map or choose)" = "", site_choices)
        )
      ),
      mainPanel(
        width = 9,
        leafletOutput("monitoring_map", height = 500),
        br(),
        uiOutput("monitoring_title"),
        plotlyOutput("monitoring_trend_plot", height = 300)
      )
    )
  ), 
  
  
  tabPanel(
    "Gridded Emissions Map",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        helpText(
          "Total benzene emissions per 1km grid cell (NAEI, all sources),",
          "shown on a log colour scale since emissions are highly skewed",
          "— a handful of cells are much higher than the rest."
        ),
        selectInput(
          "raster_year",
          "Year",
          choices = raster_year_range,
          multiple = FALSE
        ),
        selectInput(
          "raster_sources",
          "Source(s)",
          choices = raster_source_choices,
          selected = "tota",
          multiple = FALSE
        )
      ),
      mainPanel(
        width = 9,
        plotOutput("raster_map", height = 550)
      )
    )
  )
  
  
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Filtered raw points, driven by sector + year controls
  point_sources_filtered <- reactive({
    req(input$sectors)
    point_sources_all |>
      filter(
        Sector %in% input$sectors,
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      )
  })
  
  # One location per Site, used only to fit the clustering
  site_locations <- reactive({
    point_sources_filtered() |>
      group_by(Site) |>
      slice(1) |>
      ungroup()
  })
  
  # Cluster assignment, re-run whenever eps or the filtered data changes
  clustered_points <- reactive({
    sl <- site_locations()
    validate(need(nrow(sl) > 0, "No sites match the current filters."))
    
    coords <- st_coordinates(sl)
    cl <- dbscan(coords, eps = input$eps * 1000, minPts = 1)
    sl$cluster <- cl$cluster
    
    point_sources_filtered() |>
      left_join(
        sl |> 
        st_drop_geometry() |> 
        select(Site, cluster),
        by = "Site"
      )
  })
  
  # Yearly totals per cluster (used for the map + the trend plot)
  cluster_yearly <- reactive({
    ps <- clustered_points()
    coords <- st_coordinates(ps)
    
    ps |>
      mutate(x = coords[, 1], y = coords[, 2]) |>
      st_drop_geometry() |>
      group_by(cluster, Year, Sector) |>
      summarise(
        total_emissions = sum(Emission, na.rm = TRUE),
        x = weighted.mean(x, Emission),
        y = weighted.mean(y, Emission),
        n_sites = n_distinct(Site),
        .groups = "drop"
      ) |>
      st_as_sf(coords = c("x", "y"), crs = 27700)
  })
  
  # One summary point per cluster, for the map markers
  cluster_summary <- reactive({
    cy <- cluster_yearly()
    coords <- st_coordinates(cy)
    
    summary_df <- cy |>
      mutate(x = coords[, 1], y = coords[, 2]) |>
      st_drop_geometry() |>
      group_by(cluster) |>
      summarise(
        # NB: keep the summed total under a different name until after the
        # weighted means are computed — otherwise this new `total_emissions`
        # shadows the per-row column before weighted.mean() can use it as
        # weights, and x/total_emissions end up mismatched in length.
        total_emissions_summed = sum(total_emissions),
        x = weighted.mean(x, total_emissions),
        y = weighted.mean(y, total_emissions),
        n_sites = max(n_sites),
        .groups = "drop"
      ) |>
      rename(total_emissions = total_emissions_summed) |>
      st_as_sf(coords = c("x", "y"), crs = 27700) |>
      st_transform(4326)
    
    cutoff <- quantile(summary_df$total_emissions, probs = 1 - input$top_pct / 100)
    summary_df |> filter(total_emissions >= cutoff)
  })
  
  # Which cluster is currently selected (via map click)
  selected_cluster <- reactiveVal(NULL)
  
  observeEvent(input$map_marker_click, {
    selected_cluster(input$map_marker_click$id)
  })
  
  # Reset selection if it disappears from the current filtered view
  observeEvent(cluster_summary(), {
    valid_ids <- cluster_summary()$cluster
    if (!is.null(selected_cluster()) && !(selected_cluster() %in% valid_ids)) {
      selected_cluster(NULL)
    }
  })
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -2.5, lat = 54.5, zoom = 6)
  })
  
  # Update markers without redrawing the whole map (keeps zoom/pan state)
  observe({
    cs <- cluster_summary()
    
    leafletProxy("map", data = cs) |>
      clearMarkers() |>
      addCircleMarkers(
        layerId = ~cluster,
        radius = ~rescale(sqrt(total_emissions), to = c(6, 26)),
        stroke = TRUE,
        color = "#2c3e50",
        weight = 1,
        fillColor = "#e67e22",
        fillOpacity = 0.75,
        popup = ~paste0(
          "<b>Cluster ", cluster, "</b><br>",
          "Sites: ", n_sites, "<br>",
          "Total emissions: ", round(total_emissions, 1)
        )
      )
  })
  
  output$cluster_title <- renderUI({
    if (is.null(selected_cluster())) {
      h4("Select a cluster on the map to see its trend")
    } else {
      h4(paste("Cluster", selected_cluster(), "— emissions over time"))
    }
  })
  
  output$trend_plot <- renderPlotly({
    req(selected_cluster())
    
    
    ggplotly(
    cluster_yearly() |>
      st_drop_geometry() |>
      filter(cluster == selected_cluster()) |>
      filter(Sector %in% input$sectors) |>
      ggplot(aes(x = Year, y = total_emissions, colour = Sector, group = Sector)) +
      geom_line(colour = "#2c3e50") +
      geom_point(size = 2) +
      scale_y_continuous(name = "Benzene emissions", limits = c(0,NA)) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
    )
  })
  
  output$site_table <- renderTable({
    req(selected_cluster())
    
    print(names(clustered_points()))
    
    print(names(cluster_yearly())) 
    
    print(input$sectors)
    
    clustered_points() |>
      st_drop_geometry() |>
      filter(cluster == selected_cluster()) |>
      filter(`Sector` %in% input$sectors) |>
      group_by(Site, Sector) |>
      summarise(total_emissions = sum(Emission, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(total_emissions)) |>
      slice_head(n = 10) |>
      rename(
        `Site` = Site,
        `Sector` = Sector,
        `Total emissions` = total_emissions
      )
  })
  
  
  output$total_by_sector_plot <- renderPlotly(
    
    ggplotly(
      ggplot(total_by_sector_by_year) +
        geom_col(aes(x = Year, y= total_emissions, fill = Sector)) +
        theme_minimal()
    )
    
    
  )
  
  selected_site <- reactiveVal(NULL)
  
  # Marker click sets the selected site...
  observeEvent(input$monitoring_map_marker_click, {
    selected_site(input$monitoring_map_marker_click$id)
  })
  
  # ...and so does picking from the dropdown, kept in sync both ways.
  observeEvent(input$monitoring_site, {
    if (nzchar(input$monitoring_site)) {
      selected_site(input$monitoring_site)
    }
  })
  
  observeEvent(selected_site(), {
    req(selected_site())
    updateSelectInput(session, "monitoring_site", selected = selected_site())
  })
  
  output$monitoring_map <- renderLeaflet({
    leaflet(network_locations) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addCircleMarkers(
        layerId = ~`Site Name`,
        radius = 6,
        stroke = TRUE,
        color = "#2c3e50",
        weight = 1,
        fillColor = "#3498db",
        fillOpacity = 0.8,
        popup = ~`Site Name`
      )
  })
  
  output$monitoring_title <- renderUI({
    if (is.null(selected_site())) {
      h4("Select a monitoring site on the map or from the dropdown")
    } else {
      h4(paste(selected_site(), "— measured benzene"))
    }
  })
  
  output$monitoring_trend_plot <- renderPlotly({
    req(selected_site())
    
    site_data <- benzene_monitoring_data |>
      filter(site == selected_site())
    
    validate(need(nrow(site_data) > 0, "No measured data found for this site."))
    
    ggplotly(site_data |>
      ggplot(aes(x = end_date, y = benzene)) +
      geom_point(colour = "#3498db") +
      geom_line(colour = "#2c3e50") +
      geom_hline(yintercept = 5, linetype = "dashed") +
      scale_y_continuous(name = "Measured benzene micro grams per m3", limits = c(0,6)) +
      theme_minimal(base_size = 13)
    )
  })
  
  # ---------------------------------------------------------------------------
  # Gridded Map tab
  # ---------------------------------------------------------------------------
  
  # Pull just the selected year/sources from Arrow, and sum across sources
  # per grid cell — this is the step that turns "one row per source per
  # cell" into "one row per cell", which is what a raster needs.
  gridded_emissions <- reactive({
    req(input$raster_year, input$raster_sources)
    
    
    sources <- as.character(input$raster_sources)
    year <- as.integer(input$raster_year)
    
    naei_ds |>
      filter(
        year == year,
        source %in% sources,
      ) |>
      select(x, y, bz) |>
      collect() |>
      group_by(x, y) |>
      summarise(total_bz = sum(bz, na.rm = TRUE), .groups = "drop")
  })
  
  # Build the actual raster. Because bz_all is already a regular 1km grid,
  # terra::rast(..., type = "xyz") can go straight from (x, y, value) rows
  # to a raster — no interpolation needed, it just fills in the grid cells
  # at their known locations. This would NOT work directly on point-source
  # data with irregular spacing (like bz_point_sources) — that would need
  # binning into grid cells first, or a proper interpolation method.
  emissions_raster <- reactive({
    df <- gridded_emissions()
    validate(need(nrow(df) > 0, "No gridded data for this year/source selection."))
    
    r <- terra::rast(
      as.data.frame(df)[, c("x", "y", "total_bz")],
      type = "xyz",
      crs = "EPSG:27700"
    )
    
    # Cells with zero emissions read as clutter rather than signal on a UK-
    # wide map — treating them as NA lets the plot render them transparent
    # instead of a flat colour covering the whole country.
    r[r <= 0] <- NA
    
    r
  })
  
  output$raster_map <- renderPlot({
    r <- emissions_raster()
  
    print(names(gridded_emissions()))
    print(names(emissions_raster()))
  
    # terra::as.data.frame(..., xy = TRUE) gives one row per cell with
    # x, y, and the value column — this is the format geom_raster wants.
    # The value column keeps its original name from emissions_raster(),
    # "total_bz", just log-transformed.
    raster_df <- as.data.frame(r, xy = TRUE)
    validate(need(nrow(raster_df) > 0, "No non-zero cells to display."))
    
    ggplot() +
      geom_raster(data = raster_df, aes(x = x, y = y, fill = total_bz)) +
      geom_sf(data = uk_outline, fill = NA, colour = "grey30", linewidth = 0.3, inherit.aes = FALSE) +
      scale_fill_viridis_c(
        name = "Total benzene",
        na.value = "transparent",
        trans = "log1p"
        # Colour scale is on the log-transformed values (for a readable
        # spread across skewed data); labels are converted back to real
        # units so the legend still means something to a reader.
      ) +
      coord_sf(crs = 27700) +
      theme_minimal() +
      theme(axis.title = element_blank())
  })
  

  
}

shinyApp(ui, server)