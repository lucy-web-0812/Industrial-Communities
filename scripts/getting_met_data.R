# Finding closest met station... 

library(worldmet)
library(openair)


station_lng <- 344190
station_lat <- 375060
data_year <- 2023

# Produce a map of closest stations just to check...
import_ghcn_stations(lng = station_lng , lat = station_lat, crs = 27700, return = "map")


closest_stations <- import_ghcn_stations(lng = station_lng , lat = station_lat, crs = 27700, return = "table")


closest_station_code <- closest_stations|> 
  head(1) |> 
  pull(id)


closest_station_distance <- closest_stations |> 
  head(1) |> 
  pull(distance)

cat("The closest stations is", closest_station_distance, "km away")


met_data <- import_ghcn_hourly(station = closest_station_code, year = data_year)



write_adms(met_data, file= "ADMS_modelling/met_files/stanlow_2023.met")
