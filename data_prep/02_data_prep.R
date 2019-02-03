# ==================================================================================================
#                          2. Data preparation - Spatial object                                    #
# ==================================================================================================

# LIBRARIES ----------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tidylog)
library(here)
library(sp)

# READ DATA ----------------------------------------------------------------------------------------
sa_schools <- readRDS("data_prep/sa_schools.RDS")


# TODO
# Create spatial object

# sa_gis <- sa_schools
# coordinates(sa_gis) <- c("GIS_Longitude", "GIS_Latitude")

# Look up GIS coordinates where have street address