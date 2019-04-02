# ==================================================================================================
#                          2. Data preparation - Geocoding and spatial object                      #
# ==================================================================================================

# LIBRARIES ----------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tidylog)
library(here)
library(tmap)
library(ggmap)
library(sp)
library(leaflet)
library(purrr)

# Reference: https://www.r-bloggers.com/osm-nominatim-with-r-getting-locations-geo-coordinates-by-its-address/

# TODO
# Look up GIS coordinates where have street address or name of school if NA/incorrect
# Create spatial object
# coordinates(sa_gis) <- c("GIS_Longitude", "GIS_Latitude")

# READ DATA ----------------------------------------------------------------------------------------
sa_schools_01 <- readRDS("data/01_sa_schools.RDS")

# MISSING GIS --------------------------------------------------------------------------------------
gis_na <- subset(sa_schools_01, is.na(GIS_Longitude) | is.na(GIS_Latitude))
# Note: There are 610 schools without GIS coordinates. Some of these have street addresses.

# Missing GIS but have address
gis_addr <- subset(gis_na, !is.na(StreetAddress))

# Missing GIS but have town name
gis_town <- subset(gis_na, !is.na(Town_City) & !is.na(Suburb))

# NEW VARIABLE TO SEARCH ---------------------------------------------------------------------------
# Combine school and town name to search
# gis_town <- gis_town %>%
#   mutate(school_town = paste(Institution_Name, Town_City, sep = ", "))

# Combine suburb and town name to search
gis_town <- gis_town %>%
  mutate(suburb_town = paste(Suburb, Town_City, sep = ", "))

# GEOCODING ----------------------------------------------------------------------------------------
# # Create subsets for testing
# gis_addr_na_small <- gis_addr %>%
#   top_n(30)
#
# gis_town_small <- gis_town %>%
#   top_n(10)
#
# # # Using addresses
# geo_osm <- geocode_OSM(gis_addr_na_small$StreetAddress)
#
# # Using town
# geo_osm <- geocode_OSM(gis_town_small$Town_City)
#
# # Using town and school name
# geo_osm_school <- geocode_OSM(gis_town_small$school_town)

# Using town and suburb name
# gis_town <- gis_town %>%
#   mutate(geo_osm_suburb = pmap(list(q = suburb_town, as.data.frame = TRUE), geocode_OSM))
# gis_town <- gis_town %>%
#   unnest(geo_osm_suburb)

geo_osm_sub_town <- geocode_OSM(gis_town$suburb_town)

write.csv(geo_osm_sub_town, "data/geo_osm_sub_town.csv")

# PLOT GEOCODED RESULTS ----------------------------------------------------------------------------
m <- leaflet(data = geo_osm_sub_town) %>%
  addTiles() %>%
  addMarkers()
m

# BOUNDING BOX -------------------------------------------------------------------------------------
# Several are outside the boundaries of South Africa, so subset and exclude those
# Use OSM to get bounding box for SA
# bbox = min Longitude , min Latitude , max Longitude , max Latitude
# bbox = (16.3449768409, -34.8191663551, 32.830120477, -22.0913127581)
min_lon = 16.3449768409
min_lat = -34.8191663551
max_lon = 32.830120477
max_lat = -22.0913127581

geo_osm_sub_town <- geo_osm_sub_town %>%
  filter(lat >= min_lat & lat <= max_lat) %>%
  filter(lon >= min_lon & lon <= max_lon) %>%
  select(query, lat, lon)

# JOIN TOGETHER ------------------------------------------------------------------------------------
gis_town <- gis_town %>%
  left_join(geo_osm_sub_town, by = c("suburb_town" = "query")) %>%
  filter(!is.na(lat)) %>%
  group_by(NatEmis) %>%
  slice(1) %>%
  mutate(GIS_Latitude = lat,
         GIS_Longitude = lon) %>%
  select(-lat, -lon, -suburb_town) %>%
  ungroup()

# Remove then add back in
sa_schools_01a <- sa_schools_01 %>%
  anti_join(gis_town, by = "NatEmis")

sa_schools_01 <- rbind(sa_schools_01a, gis_town)

# REMOVE MISSING GIS -------------------------------------------------------------------------------
sa_schools_02 <- sa_schools_01 %>%
  filter(!is.na(GIS_Longitude) | !is.na(GIS_Latitude))

# CONVERT TO SPATIAL POINTS ------------------------------------------------------------------------
# Rename columns
sa_schools_02 <- sa_schools_02 %>%
  mutate(latitude = GIS_Latitude,
         longitude = GIS_Longitude) %>%
  select(-GIS_Longitude, -GIS_Latitude)

# SAVE ---------------------------------------------------------------------------------------------
saveRDS(sa_schools_02, "data/02_sa_schools.RDS")
