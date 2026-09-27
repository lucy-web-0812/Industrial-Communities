# =============================================================================
# Combined map: benzene monitoring stations + major point sources
#
# One leaflet map, two marker layers:
#   - Monitoring stations (NAHN network)      -> click shows measured benzene
#     time series for that site
#   - Point source clusters (NAEI point data) -> click shows the emissions
#     trend (per site) for that cluster
#
# Point sources are clustered with DBSCAN before plotting (same approach as
# ../benzene_app.R) because there are ~2,900 distinct point-source sites --
# far too many to plot individually without the map turning into a solid
# blob of markers. Clicking a cluster then drills down to the individual
# sites within it.
#
# Marker identity is encoded in the leaflet layerId as "MON|<site name>" or
# "PT|<cluster id>", so a single observeEvent on input$map_marker_click can
# tell the two apart and route to the right plot.
# =============================================================================

library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(plotly)
library(dbscan)
library(scales)

# -----------------------------------------------------------------------------
# 1. Load data once at app start
# -----------------------------------------------------------------------------

# ---- Point sources (already UK-filtered — see scripts/point_sources.R) ----
point_sources_all <- read_rds("processed_data/point_sources.rds")

# ---- Monitoring stations ----
network_locations <- read_csv("data/measured_data/all_NAHN_locations.csv", show_col_types = FALSE) |>
  select(`Site Name`, Latitude, Longitude, `End Date`) |>
  rename(lng = Longitude, lat = Latitude) |>
  mutate(
    `Site Name` = case_when(
      `Site Name` == "Birmingham Centre" ~ "Birmingham Roadside",
      `Site Name` == "Camden Kerbside" ~ "Camden Kerbside(Swiss Cottage)",
      `Site Name` == "Leeds Headingley Kerbside" ~ "Leeds Roadside",
      .default = `Site Name`
    ),
    still_active = is.na(`End Date`)
  ) |>
  st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = FALSE)

monitoring_data_raw <- read_csv(
  "data/measured_data/NAHN_measured_data_all.csv",
  col_names = FALSE, show_col_types = FALSE
)
names(monitoring_data_raw) <- c("col1", "col2", "col3", "col4")

monitoring_data <- monitoring_data_raw |>
  mutate(year = str_extract(col1, "(?<=Data for year )\\d{4}")) |>
  fill(year)

site_rows <- which(monitoring_data$col1 == "Multi-day data")
monitoring_data$site <- NA_character_
monitoring_data$site[site_rows + 1] <- monitoring_data$col1[site_rows + 1]
monitoring_data <- monitoring_data |> fill(site)

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

# -----------------------------------------------------------------------------
# 2. Cluster the point sources (DBSCAN on site locations, fixed 10km radius)
#    -- one marker per cluster, sized by total emissions since 2005
# -----------------------------------------------------------------------------

site_locations <- point_sources_all |> group_by(Site) |> slice(1) |> ungroup()
coords <- st_coordinates(site_locations)
cl <- dbscan(coords, eps = 10000, minPts = 1)
site_locations$cluster <- cl$cluster

clustered_points <- point_sources_all |>
  left_join(site_locations |> st_drop_geometry() |> select(Site, cluster), by = "Site")

coords2 <- st_coordinates(clustered_points)
cluster_yearly <- clustered_points |>
  mutate(x = coords2[, 1], y = coords2[, 2]) |>
  st_drop_geometry() |>
  group_by(cluster, Year, Site) |>
  summarise(
    total_emissions = sum(Emission, na.rm = TRUE),
    x = weighted.mean(x, Emission),
    y = weighted.mean(y, Emission),
    .groups = "drop"
  )

coords3 <- st_coordinates(st_as_sf(cluster_yearly, coords = c("x", "y"), crs = 27700))

# NB: the summed column is named total_emissions_summed (not total_emissions)
# until AFTER the weighted means below -- weighted.mean(x, total_emissions)
# needs the original per-row column as its weight vector, and naming the new
# sum "total_emissions" too early would shadow it before that happens.
cluster_summary <- cluster_yearly |>
  mutate(x = coords3[, 1], y = coords3[, 2]) |>
  group_by(cluster) |>
  summarise(
    total_emissions_summed = sum(total_emissions),
    x = weighted.mean(x, total_emissions),
    y = weighted.mean(y, total_emissions),
    n_sites = n_distinct(Site),
    .groups = "drop"
  ) |>
  rename(total_emissions = total_emissions_summed) |>
  st_as_sf(coords = c("x", "y"), crs = 27700) |>
  st_transform(4326)

# -----------------------------------------------------------------------------
# 3. UI
# -----------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Benzene: Monitoring Sites & Point Source Clusters"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4(textOutput("panel_title")),
      uiOutput("panel_subtitle"),
      plotlyOutput("detail_plot", height = "320px"),
      conditionalPanel(
        "output.show_table == 'true'",
        tableOutput("site_table")
      ),
      hr(),
      tags$small(
        "Blue circles = monitoring stations (measured benzene, filled = still active). ",
        "Orange circles = point source clusters, sized by total emissions since 2005. ",
        "Click a marker to see its data here."
      )
    ),
    mainPanel(
      width = 8,
      leafletOutput("map", height = "700px")
    )
  )
)

# -----------------------------------------------------------------------------
# 4. Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  monitoring_col_pal <- colorFactor(palette = c("red", "#2B5C8F"), domain = network_locations$still_active)
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -2.5, lat = 54.5, zoom = 6) |>
      addCircleMarkers(
        data = network_locations,
        layerId = ~paste0("MON|", `Site Name`),
        label = ~`Site Name`,
        radius = 6,
        color = "#2c3e50",
        weight = 1,
        fillColor = ~monitoring_col_pal(still_active),
        fillOpacity = 0.85,
        group = "Monitoring stations"
      ) |>
      addCircleMarkers(
        data = cluster_summary,
        layerId = ~paste0("PT|", cluster),
        label = ~paste0(n_sites, " site(s), ", round(total_emissions, 1), " kg total"),
        radius = ~rescale(sqrt(total_emissions), to = c(6, 26)),
        color = "#2c3e50",
        weight = 1,
        fillColor = "#FC8D62",
        fillOpacity = 0.75,
        group = "Point source clusters"
      ) |>
      addLayersControl(
        overlayGroups = c("Monitoring stations", "Point source clusters"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # ---- Track what's currently selected ----
  selected <- reactiveVal(NULL)  # list(type = "MON"/"PT", id = "...")
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    req(click$id)
    parts <- str_split_fixed(click$id, "\\|", 2)
    selected(list(type = parts[1, 1], id = parts[1, 2]))
  })
  
  output$show_table <- reactive({
    sel <- selected()
    if (is.null(sel)) "false" else if (sel$type == "PT") "true" else "false"
  })
  outputOptions(output, "show_table", suspendWhenHidden = FALSE)
  
  # ---- Panel title / subtitle ----
  output$panel_title <- renderText({
    sel <- selected()
    if (is.null(sel)) return("No site selected")
    if (sel$type == "MON") {
      sel$id
    } else {
      paste("Point source cluster", sel$id)
    }
  })
  
  output$panel_subtitle <- renderUI({
    sel <- selected()
    if (is.null(sel)) {
      return(tags$p("Click a marker on the map to see its data."))
    }
    if (sel$type == "MON") {
      tags$p("Measured ambient benzene (monitoring network)")
    } else {
      tags$p("Reported point source benzene emissions, by site within this cluster (NAEI)")
    }
  })
  
  # ---- Detail plot: routes to monitoring or point-source data ----
  output$detail_plot <- renderPlotly({
    sel <- selected()
    req(sel)
    
    if (sel$type == "MON") {
      df <- benzene_monitoring_data |> filter(site == sel$id)
      
      if (nrow(df) == 0) {
        return(plotly_empty() |> layout(title = "No monitoring data for this site"))
      }
      
      p <- ggplot(df, aes(x = end_date, y = benzene)) +
        geom_line(colour = "#2c3e50") +
        geom_point(colour = "#2B5C8F", size = 1.5) +
        scale_y_continuous(name = "Benzene (\u00b5g/m\u00b3)", limits = c(0, NA)) +
        scale_x_date(name = NULL) +
        theme_minimal(base_size = 12)
      
      ggplotly(p)
      
    } else {
      df <- cluster_yearly |> filter(cluster == as.integer(sel$id))
      
      if (nrow(df) == 0) {
        return(plotly_empty() |> layout(title = "No emissions data for this cluster"))
      }
      
      p <- ggplot(df, aes(x = Year, y = total_emissions, colour = Site, group = Site)) +
        geom_line() +
        geom_point(size = 1.5) +
        scale_y_continuous(name = "Emissions (kg)", limits = c(0, NA)) +
        scale_x_continuous(name = NULL) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")
      
      ggplotly(p)
    }
  })
  
  # ---- Top sites table (point source clusters only) ----
  output$site_table <- renderTable({
    sel <- selected()
    req(sel)
    if (sel$type != "PT") return(NULL)
    
    cluster_yearly |>
      filter(cluster == as.integer(sel$id)) |>
      group_by(Site) |>
      summarise(total_emissions = sum(total_emissions, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(total_emissions)) |>
      slice_head(n = 10) |>
      rename(
        `Site` = Site,
        `Total emissions (kg, all years)` = total_emissions
      )
  })
}

shinyApp(ui, server)