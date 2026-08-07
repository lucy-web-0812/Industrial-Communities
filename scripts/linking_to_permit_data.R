# Now need to link in permit data so can see what we are dealing with.....


library(tidyverse)
library(sf)

permits_full_data <- read_csv("data/permits/industrial_installation_permits.csv")  |> 
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) |> 
  mutate(permission_date = dmy(`Permission Date`))


point_sources <- read_rds("processed_data/point_sources.rds")



collated_permit_data <- point_sources |> 
  st_join(permits_full_data, join = st_intersects) |> 
  mutate(Year = as.Date(paste0(Year, "-01-01"))) |> 
  mutate(date_flag = ifelse(permission_date < Year, TRUE, FALSE)) |> 
  filter(date_flag == T)

         