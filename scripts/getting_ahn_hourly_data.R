# AHN monitoring data download.... 

library(tidyverse)
library(openair)



# Currently just getting the data for the open sites

# Automatic: one row per hour, columns: site, date (POSIXct), benzene

automatic_sites <- importMeta(source = "aurn", all = TRUE) |>
  filter(str_detect(site, "Marylebone|Eltham|Auchencorth|Chilbolton|Honor Oak")) |>
  distinct(code)

# Download hourly data, including hydrocarbons (Marylebone Road is "my1")
ahn_hourly <- importUKAQ(site = automatic_sites$code, year = 2010:2023, hc = TRUE, pollutant = "benzene")

write_rds(ahn_hourly, "data/measured_data/ahn_hourly.rds") 