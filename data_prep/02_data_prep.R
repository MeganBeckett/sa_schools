# ==================================================================================================
#                          2. Data preparation - Spatial object                                    #
# ==================================================================================================

# LIBRARIES ----------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tidylog)
library(here)
library(sp)

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



