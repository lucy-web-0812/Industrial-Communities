#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(tidyverse)
library(sf)
library(arrow)
library(dbscan)
library(rnaturalearth)
library(leaflet)


# Pre processing.... 

point_sources_raw <- open_dataset("G:/My Drive/Year 3/Industrial Communities/processed_data/bz_point_sources")


uk_outline <- ne_countries(
  country = "United Kingdom",
  scale = "large",
  returnclass = "sf"
) |>
  st_transform(27700) |>
  select(geometry)




point_sources <- point_sources_raw |>
  select(
    Site,
    Sector,
    Year,
    Emission,
    Easting,
    Northing
  ) |>
  collect() |>
  st_as_sf(
    coords = c("Easting", "Northing"),
    crs = 27700
  ) |>
  st_filter(uk_outline)



site_locations <- point_sources |>
  group_by(Site) |>
  slice(1) |>
  ungroup()

coords <- st_coordinates(site_locations)

cl <- dbscan(
  coords,
  eps = 2000,
  minPts = 1
)

site_locations$cluster <- cl$cluster

# -----------------------------------------------------------------------------
# Join cluster IDs back to full dataset
# -----------------------------------------------------------------------------

point_sources <- point_sources |>
  left_join(
    site_locations |>
      st_drop_geometry() |>
      select(Site, cluster),
    by = "Site"
  )

# -----------------------------------------------------------------------------
# Create yearly cluster emissions
# -----------------------------------------------------------------------------

coords <- st_coordinates(point_sources)

point_sources_clustered <- point_sources |>
  mutate(
    x = coords[, 1],
    y = coords[, 2]
  ) |>
  st_drop_geometry() |>
  group_by(cluster, Year) |>
  summarise(
    total_emissions = sum(Emission),
    
    x = weighted.mean(x, Emission),
    y = weighted.mean(y, Emission),
    
    n_sites = n_distinct(Site),
    
    .groups = "drop"
  ) |>
  st_as_sf(
    coords = c("x", "y"),
    crs = 27700
  )

# -----------------------------------------------------------------------------
# One point per cluster for leaflet
# -----------------------------------------------------------------------------

coords <- st_coordinates(point_sources_clustered)

cluster_locations <- point_sources_clustered |>
  mutate(
    x = coords[,1],
    y = coords[,2]
  ) |>
  st_drop_geometry() |>
  group_by(cluster) |>
  summarise(
    total_emissions_since_2005 = sum(total_emissions),
    
    x = weighted.mean(x, total_emissions),
    y = weighted.mean(y, total_emissions),
    
    n_sites = max(n_sites),
    
    .groups = "drop"
  ) |>
  st_as_sf(
    coords = c("x","y"),
    crs = 27700
  ) |> 
  st_transform(4326)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Benzene Data"),
     
    mainPanel(

    leafletOutput("map_plot", height = 300), 
    
    plotlyOutput("timeseries"), 
    
    tableOutput("site_lists")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map_plot <- renderLeaflet({
      leaflet(cluster_locations) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addCircleMarkers(
          layerId = ~cluster,
          radius = ~scales::rescale(sqrt(total_emissions_since_2005), to = c(4,35)),
          stroke = TRUE,
          color = "pink",
          weight = 1,
          fillOpacity = 0.8,
          popup = ~paste0(
            "<b>Cluster ", cluster, "</b><br>",
            "Sites: ", n_sites, "<br>",
            "Total emissions: ",
            round(total_emissions_since_2005,1)
          )
        )
    })
    
    
    
    selected_cluster <- reactiveVal(NULL)
    
    observeEvent(input$map_plot_marker_click, {

      selected_cluster(
        as.integer(input$map_plot_marker_click$id)
      )
      
    })
    
    
    
    output$timeseries <- renderPlotly({
      
      req(selected_cluster())
      
      ggplotly(
      point_sources |>
        filter(cluster == selected_cluster()) |>
        ggplot(aes(Year, Emission, colour = Site)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(limits = c(0,NA), name = "Benzene Emissions")
      )
      
    })
    
    
    output$site_lists <- renderTable({
      
      req(selected_cluster())
      
      point_sources |> 
        filter(cluster == selected_cluster()) |> 
        distinct(Site) 
      
      
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
