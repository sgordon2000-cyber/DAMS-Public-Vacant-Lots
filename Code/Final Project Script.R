library(tidyverse)
library(sf)
library(janitor)
library(tidyr)

pluto_raw <- read.csv("/Users/EmmaNelson/Desktop/Seth's R Project (luv u emma)/pluto_25v3_1.csv")
pluto_filtered <- pluto_raw %>%
  filter(
    landuse == "11",
    ownertype != "P" | is.na(ownertype),
    # Check if ANY of the first 2 zoning columns start with "R"
    str_detect(replace_na(zonedist1, ""), "^R") | 
      str_detect(replace_na(zonedist2, ""), "^R") 
  ) %>%
  # Remove rows with missing coordinates to prevent spatial errors
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  # Convert to Spatial Object (Lat/Lon initially)
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

#loading subway stop data
subways <- read_csv("/Users/EmmaNelson/Desktop/Seth's R Project (luv u emma)/MTA_Subway_Stations.csv") %>% clean_names() %>%
  filter(!is.na(gtfs_latitude) & !is.na(gtfs_longitude)) %>%
  st_as_sf(coords = c("gtfs_longitude", "gtfs_latitude"), crs = 4326)

#transforming data to local CRS
pluto_2263 <- st_transform(pluto_filtered, 2263)
subways_2263 <- st_transform(subways, 2263)

# Create a single merged buffer shape 2640 feet (0.5 miles) around all stations
subway_buffer <- st_buffer(subways_2263, dist = 2640) %>% st_union()

# Keep only PLUTO lots that intersect with that subway buffer
pluto_near_subway <- st_filter(pluto_2263, subway_buffer)

HPD_FACTOR <- 900 # Avg sq ft per unit (family size + circulation)

pluto_sized_filtered <- pluto_near_subway %>% 
  filter(
    #filtering by min lot area by zoning (R1 vs R2 vs R3 etc...)
    # R1 Districts (Min 5,700 sq ft) ---
    (str_detect(zonedist1, "^R1") & lotarea >= 5700) |
            (str_detect(zonedist1, "^R2") & lotarea >= 3800) |
      (str_detect(zonedist1, "^R[3-5]") & lotarea >= 1700) |
      (str_detect(zonedist1, "^R[6-9]|^R10") & lotarea >= 1700)
  )

# Check how many lots remain
print(nrow(pluto_sized_filtered))

pluto_hpd_analysis <- pluto_sized_filtered %>%
  mutate(
    potential_hpd_units = case_when(
      # Low Density Calculation
      # R3-2 allows multi-family but requires 625 sq ft land per unit
      str_detect(zonedist1, "^R3-2") ~ floor(lotarea / 625),
      
      # Generic R3 (R3-1, R3A, etc) - conservative estimate
      str_detect(zonedist1, "^R3") ~ floor(lotarea / 1200),
      
      # R4 & R5 require ~900 sq ft land per unit
      str_detect(zonedist1, "^R4|^R5") ~ floor(lotarea / 900),
      
      # R1 & R2 are single family (1 unit max)
      str_detect(zonedist1, "^R1|^R2") ~ 1,
      
      # High Density Calculation
      # R6 through R10: Use (Lot Area * FAR) / Unit Size
      str_detect(zonedist1, "^R[6-9]|^R10") ~ floor((lotarea * residfar) / HPD_FACTOR),
      
      # Default fallback
      TRUE ~ 0
    ),
    
    # Calculate Net New Units (Potential - Existing)
    net_new_units = potential_hpd_units - replace_na(unitsres, 0)
  ) %>%
  # Remove results with 0 capacity
  filter(net_new_units > 0)

#create stats for reporting
borough_stats <- pluto_hpd_analysis %>%
  st_drop_geometry() %>% 
  group_by(borough) %>%
  summarize(
    Total_Potential_Units = sum(net_new_units, na.rm=TRUE),
    Total_Vacant_Sites = n()
  ) %>%
  arrange(desc(Total_Potential_Units))

# View final table
print(borough_stats)

#create pluto dataset for QGIS tabular join for heatmap
pluto_clean_export <- pluto_filtered %>%
  st_transform(4326) %>%
  mutate(
    clean_longitude = st_coordinates(.)[,1],
    clean_latitude  = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry
nrow(pluto_clean_export)
write_csv(pluto_clean_export, "/Users/EmmaNelson/Desktop/pluto_clean_for_qgis.csv")


