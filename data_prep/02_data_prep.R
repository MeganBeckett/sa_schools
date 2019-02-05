# ==================================================================================================
#                          2. Data preparation - Spatial object                                    #
# ==================================================================================================

# LIBRARIES ----------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tidylog)
library(here)
library(tmaptools)
library(ggmap)

# Reference: https://www.r-bloggers.com/osm-nominatim-with-r-getting-locations-geo-coordinates-by-its-address/

# TODO
# Look up GIS coordinates where have street address or name of school if NA/incorrect
# Create spatial object
# coordinates(sa_gis) <- c("GIS_Longitude", "GIS_Latitude")

# READ DATA ----------------------------------------------------------------------------------------
sa_schools <- readRDS("data_prep/01_sa_schools.RDS")

# MISSING GIS --------------------------------------------------------------------------------------
gis_na <- subset(sa_schools, is.na(GIS_Longitude) | is.na(GIS_Latitude))
# Note: There are 610 schools without GIS coordinates. Some of these have street addresses.

# Missing GIS but have address
gis_addr_na <- subset(gis_na, !is.na(StreetAddress))

# Missing GIS but have town name
gis_town_na <- subset(gis_na, !is.na(Town_City))

# combine schol and town name to search
gis_town_na <- gis_town_na %>%
  mutate(school_town = paste(Institution_Name, Town_City, sep = ", "))


# GEOCODING ----------------------------------------------------------------------------------------
# ggmap not working as haven't registered API key -> requires credit card details, hencie OSM!
# Create subsets for testing
gis_addr_na_small <- gis_addr_na %>%
  top_n(30)

gis_town_small <- gis_town_na %>%
  top_n(10)

# Using addresses
geo_osm <- geocode_OSM(gis_addr_na_small$StreetAddress)

geo_gg <- ggmap::geocode(gis_addr_na_small$StreetAddress)

# Using town
geo_osm <- geocode_OSM(gis_town_small$Town_City)

geo_gg <- ggmap::geocode(gis_town_small$Town_City, source = "google")

# Using town and school name
geo_osm_school <- geocode_OSM(gis_town_small$school_town)

geo_gg_school <- geocode(gis_town_small$school_town, source = "google", sensor = TRUE)
